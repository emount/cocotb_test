-- File Name   : Bayer_Demosaic_Encapsulator.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Submodule responsible for encapsulation of Bayer-demosaiced
--               video payloads with headers containing run-time format details.
--
--               Output headers are simply replicated as identical to those of
--               the input, time-delayed to align with the systolic latency of
--               the demosaicing pipeline.
--
--     o  0
--     | /       Copyright (c) 2017-2018
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE Standard Logic libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Module packages
library work;
use work.Bayer_Demosaic_Package.all;
use work.CL_Video_Defs.all;
use work.Hdl_Utilities.all;


-- Entity declaration
entity Bayer_Demosaic_Encapsulator is
  generic
  (
    -- FPGA_FAMILY       - String indicating the FPGA family to compile for
    -- PEL_DEPTH         - Depth of each pixel element, in bits
    -- PIXELS_PER_CLOCK  - Number of horizontal pixel values per clock
    -- EMPTY_SYMBOL_BITS - Width of the input "empty symbols" vector, in bits
    -- BYPASS_ELASTIC    - Flag indicating whether to bypass backpressure logic
    FPGA_FAMILY       : string;
    PEL_DEPTH         : positive;
    PIXELS_PER_CLOCK  : positive;
    EMPTY_SYMBOL_BITS : positive;
    BYPASS_ELASTIC    : boolean
  );
  port
  (
    -- Pixel reset and clock
    Pixel_Reset : in std_logic;
    Pixel_Clock : in std_logic;

    -- Header fields and triggering interface
    Header_Trigger    : in std_logic;
    Header_Resolution : in CL_Video_Coordinate;
    Header_Roi_Offset : in CL_Video_Coordinate;
    Header_Index      : in CL_Video_Frame_Index;
    Header_Timestamp  : in CL_Video_Frame_Timestamp;

    -- Bayer-processed pixel stream
    Bayer_Valid : in std_logic;
    Bayer_Red   : in std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    Bayer_Green : in std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    Bayer_Blue  : in std_logic_vector(((PIXELS_PER_CLOCK * PEL_DEPTH) - 1) downto 0);
    Bayer_Start : in std_logic;
    Bayer_End   : in std_logic;

    -- Pipelined backpressure indication
    Sink_Ready : out std_logic;

    -- Output pixel stream
    Output_Valid         : out std_logic;
    Output_Data          : out std_logic_vector(((PIXELS_PER_CLOCK * NUM_RGB_PLANES * PEL_LANE_BITS) - 1) downto 0);
    Output_StartOfPacket : out std_logic;
    Output_EndOfPacket   : out std_logic;
    Output_Empty         : out std_logic_vector((EMPTY_SYMBOL_BITS - 1) downto 0);
    Output_Ready         : in  std_logic
  );
end entity;


-- Architecture implementation
architecture rtl of Bayer_Demosaic_Encapsulator is
  
  -- TODO - These are replicated verbatim from the parser module; these should
  --        be implemented using a VHDL-2008 package with generics, but GHDL is
  --        crashing on this construct.
  --
  -- Computed constants for slicing the header bit fields
  --
  -- The width of each header field and each pixel field are identical; this
  -- is used as a priori knowledge, rather than defining a more-complex
  -- relationship.
  --
  -- There is no existing use case / leeway in the frame packing spec to
  -- require otherwise.
  constant FIELDS_PER_CLOCK       : positive := PIXELS_PER_CLOCK;
  constant FRAME_RES_WORD         : natural  := (CL_VIDEO_FRAME_WIDTH_FIELD / FIELDS_PER_CLOCK);
  constant FRAME_WIDTH_LOW        : natural  := ((CL_VIDEO_FRAME_WIDTH_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant FRAME_HEIGHT_LOW       : natural  := ((CL_VIDEO_FRAME_HEIGHT_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant FRAME_ROI_OFFSET_WORD  : natural  := (CL_VIDEO_ROI_OFFSET_X_FIELD / FIELDS_PER_CLOCK);
  constant FRAME_ROI_OFFSET_X_LOW : natural  := ((CL_VIDEO_ROI_OFFSET_X_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant FRAME_ROI_OFFSET_Y_LOW : natural  := ((CL_VIDEO_ROI_OFFSET_Y_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant FRAME_INDEX_WORD       : natural  := (CL_VIDEO_FRAME_INDEX_FIELD / FIELDS_PER_CLOCK);
  constant FRAME_INDEX_LOW        : natural  := ((CL_VIDEO_FRAME_INDEX_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant FRAME_TIMESTAMP_WORD   : natural  := (CL_VIDEO_TIMESTAMP_FIELD / FIELDS_PER_CLOCK);
  constant FRAME_TIMESTAMP_LOW    : natural  := ((CL_VIDEO_TIMESTAMP_FIELD mod FIELDS_PER_CLOCK) * CL_VIDEO_HEADER_FIELD_BITS);
  constant PAYLOAD_START_WORD     : positive := (CL_VIDEO_PAYLOAD_START_FIELD / FIELDS_PER_CLOCK);

  -- Header generation state space
  constant WORD_COUNT_BITS  : positive        := Min_Repr_Bits(PAYLOAD_START_WORD - 1);
  subtype Word_Count_Type is unsigned((WORD_COUNT_BITS - 1) downto 0);
  constant PACK_WIDTH       : positive        := (PIXELS_PER_CLOCK * PEL_LANE_BITS);
  signal header_Count       : Word_Count_Type := To_Unsigned(0, Word_Count_Type'LENGTH);
  signal header_Count_tc    : std_logic;
  signal header_Armed       : std_logic;
  signal header_Cycle       : std_logic;

  -- Pipeline signals
  signal bayer_Data         : std_logic_vector(Output_Data'RANGE);
  signal bayer_Valid_d      : std_logic;
  signal bayer_Start_d      : std_logic;
  signal bayer_End_d        : std_logic;
  signal bayer_Data_d       : std_logic_vector(Output_Data'RANGE);
  signal encapsulated_Valid : std_logic;
  signal encapsulated_Start : std_logic;
  signal encapsulated_End   : std_logic;
  signal encapsulated_Data  : std_logic_vector(Output_Data'RANGE); 

begin

  -- Aggregate the color planes into a single vector for stream output
  pack_Planes : process(all)
    -- Compute the least-significant bit index of actual pel data within each
    -- lane of output pel values, which are maximally-sized
    constant PEL_SLICE_LSB   : natural := (PEL_LANE_BITS - PEL_DEPTH);
    variable output_Pel_High : positive;
    variable output_Pel_Low  : natural;
  begin
    -- Pack the pixels as RGB using big-endian order; perform the expansion,
    -- if any, of the pipeline pels to external pel-lane bits, now that they
    -- are to be interleaved with header encapsulation words and presented to
    -- the backpressure management FIFO.
    --
    -- Assign all bits below the least-significant bit of input pixel data
    -- with zeros for padding
    bayer_Data <= (others => '0');
    for pixel_Index in 0 to (PIXELS_PER_CLOCK - 1) loop
      -- Compute the bounds  within the output vector for the first pel of
      -- the pixel value
      output_Pel_High := ((((pixel_Index * NUM_RGB_PLANES) + 1) * PEL_LANE_BITS) - 1);
      output_Pel_Low  := ((pixel_Index * NUM_RGB_PLANES * PEL_LANE_BITS) + PEL_SLICE_LSB);

      -- Iterate through each color plane, assigning output
      for plane_Index in 0 to (NUM_RGB_PLANES - 1) loop
        -- Pel data are MSB-justified within the output pel lanes
        case plane_Index is
          when RGB_PLANE_RED =>
            bayer_Data((output_Pel_High + (plane_Index * PEL_LANE_BITS)) downto
                       (output_Pel_Low + (plane_Index * PEL_LANE_BITS))) <=
              Bayer_Red((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH));
              
          when RGB_PLANE_GREEN =>
            bayer_Data((output_Pel_High + (plane_Index * PEL_LANE_BITS)) downto
                       (output_Pel_Low + (plane_Index * PEL_LANE_BITS))) <=
              Bayer_Green((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH));

          when others =>
            bayer_Data((output_Pel_High + (plane_Index * PEL_LANE_BITS)) downto
                       (output_Pel_Low + (plane_Index * PEL_LANE_BITS))) <=
              Bayer_Blue((((pixel_Index + 1) * PEL_DEPTH) - 1) downto (pixel_Index * PEL_DEPTH));

        end case;
      end loop;
    end loop;
  end process;
  
  
  --
  -- Process generating header words for stitching with the Bayer-processed
  -- pixel data
  --
  header_Generate : process(Pixel_Reset, Pixel_Clock)

    -- Function returning a packed vector containing header fields
    -- for an active header cycle
    --
    -- \param header_Beat - Cardinal number of the header data beat to pack
    impure function Pack_Header_Fields(header_Beat : in natural) return std_logic_vector is
      variable return_Vector : std_logic_vector((PACK_WIDTH - 1) downto 0);
      variable slice_High    : positive;
      variable slice_Low     : natural;
      variable field_Offset  : natural;
    begin

      -- Loop over all the fields to be packed on this data beat
      for field_Index in 0 to (FIELDS_PER_CLOCK - 1) loop
        
        -- Compute the bit slice to pack on this iteration, packing using
        -- little-endian field ordering
        slice_Low  := (field_Index * PEL_LANE_BITS);
        slice_High := (slice_Low + PEL_LANE_BITS - 1);

        -- Multiplex in the correct header field value to each slice
        field_Offset := ((header_Beat * PIXELS_PER_CLOCK) + field_Index);

        case field_Offset is
          when CL_VIDEO_FRAME_WIDTH_FIELD       => return_Vector(slice_High downto slice_Low) := std_logic_vector(Header_Resolution.x);
          when CL_VIDEO_FRAME_HEIGHT_FIELD      => return_Vector(slice_High downto slice_Low) := std_logic_vector(Header_Resolution.y);
          when CL_VIDEO_ROI_OFFSET_X_FIELD      => return_Vector(slice_High downto slice_Low) := std_logic_vector(Header_Roi_Offset.x);
          when CL_VIDEO_ROI_OFFSET_Y_FIELD      => return_Vector(slice_High downto slice_Low) := std_logic_vector(Header_Roi_Offset.y);
          when CL_VIDEO_FRAME_INDEX_FIELD       => return_Vector(slice_High downto slice_Low) := std_logic_vector(Header_Index((PEL_LANE_BITS - 1) downto 0));
          when (CL_VIDEO_FRAME_INDEX_FIELD + 1) => return_Vector(slice_High downto slice_Low) := std_logic_vector(Header_Index(Header_Index'HIGH downto PEL_LANE_BITS));
          when CL_VIDEO_TIMESTAMP_FIELD         => return_Vector(slice_High downto slice_Low) := std_logic_vector(Header_Timestamp((PEL_LANE_BITS - 1) downto 0));
          when (CL_VIDEO_TIMESTAMP_FIELD + 1)   => return_Vector(slice_High downto slice_Low) := std_logic_vector(Header_Timestamp(Header_Index'HIGH downto PEL_LANE_BITS));
          when others =>
            -- Encode null data for all spare fields
            return_Vector(slice_High downto slice_Low) := (others => '0');

        end case;
        
      end loop;

      return return_Vector;

    end function;

    -- Header word vector
    variable pack_Vector : std_logic_vector((PACK_WIDTH - 1) downto 0);
    
  begin
    if ?? Pixel_Reset then
      header_Count       <= To_Unsigned(0, header_Count'LENGTH);
      header_Count_tc    <= '0';
      header_Armed       <= '0';
      header_Cycle       <= '0';
      encapsulated_Valid <= '0';
    elsif rising_edge(Pixel_Clock) then

      -- Default output assignments
      encapsulated_Valid <= '0';
      encapsulated_Start <= '0';
      encapsulated_End   <= '0';
      encapsulated_Data  <= (others => '0');

      -- Detect the cycle in which the input header values are captured. It
      -- is assumed that the header inputs remain stable for at least as long
      -- as the header generation
      if ?? Header_Trigger then
        -- Begin the header generation sequence
        header_Armed <= '1';
        header_Cycle <= '1';
        header_Count <= To_Unsigned(0, header_Count'LENGTH);
        if(PAYLOAD_START_WORD = 1) then
          header_Count_tc <= '1';
        else
          header_Count_tc <= '0';
        end if;

        -- Conditionally pack header fields into the output vector, starting
        -- the output packet
        encapsulated_Valid <= '1';
        encapsulated_Start <= '1';

        -- TODO - For now, simply replicate the header fields at each
        --        color plane; need to nail down what is actually expected.
        pack_Vector := Pack_Header_Fields(0);
        for plane_Index in 0 to (NUM_RGB_PLANES - 1) loop
          encapsulated_Data((((plane_Index + 1) * PACK_WIDTH) - 1) downto
                            (plane_Index * PACK_WIDTH)) <= pack_Vector;
        end loop;
      elsif ?? (header_Armed and not header_Count_tc) then
        -- Continue with additional header data beats
        header_Count <= (header_Count + 1);
        if(PAYLOAD_START_WORD >= 2) then
          if(header_Count = (PAYLOAD_START_WORD - 2)) then
            header_Count_tc <= '1';
          end if;
        else
          header_Count_tc <= '1';
        end if;

        -- Pack data for the next header beat
        encapsulated_Valid <= '1';
        pack_Vector        := Pack_Header_Fields(To_Integer(header_Count) + 1);
        for plane_Index in 0 to (NUM_RGB_PLANES - 1) loop
          encapsulated_Data((((plane_Index + 1) * PACK_WIDTH) - 1) downto
                            (plane_Index * PACK_WIDTH)) <= pack_Vector;
        end loop;
      else
        -- Disarm and deactivate header generation upon state entry
        header_Armed <= '0';
        header_Cycle <= '0';
        
        -- Forward along encapsulated video data as it is enabled
        encapsulated_Valid <= Bayer_Valid;
        encapsulated_End   <= Bayer_End;
        encapsulated_Data  <= bayer_Data;
      end if; -- if(header trigger / header cycle)

    end if;
  end process;
  
  -- None of the symbols are empty in any data beat
  Output_Empty <= (others => '0');
  
  
  --
  -- Instantiate the elastic logic in order to manage backpressure
  --
  elastic_Logic : entity work.Bayer_Demosaic_Elastic
  generic map
  (
    FPGA_FAMILY    => FPGA_FAMILY,
    DATA_WIDTH     => Output_Data'LENGTH,
    BYPASS_ELASTIC => BYPASS_ELASTIC
  )
  port map
  (
    -- Pixel reset and clock
    Pixel_Reset => Pixel_Reset,
    Pixel_Clock => Pixel_Clock,
    
    -- Input pixel stream
    Input_Valid => encapsulated_Valid,
    Input_Data  => encapsulated_Data,
    Input_Start => encapsulated_Start,
    Input_End   => encapsulated_End,
    Input_Ready => Sink_Ready,
      
    -- Output pixel stream
    Output_Valid  => Output_Valid,
    Output_Start  => Output_StartOfPacket,
    Output_End    => Output_EndOfPacket,
    Output_Data   => Output_Data,
    Output_Ready  => Output_Ready
  );
  
end architecture;
