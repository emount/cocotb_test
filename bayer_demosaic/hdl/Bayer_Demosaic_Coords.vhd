-- File Name   : Bayer_Demosaic_Coords.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Submodule generating coordinate pairs associated with
--               the pixel data of an ingress video stream.
--
--     o  0
--     | /       Copyright (c) 2017
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE Standard Logic libraries
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

-- Module packages
library work;
use work.CL_Video_Defs.all;


-- Entity declaration
entity Bayer_Demosaic_Coords is
  generic
  (
    -- PIXELS_PER_CLOCK - Number of horizontal pixel values per clock
    PIXELS_PER_CLOCK : positive
  );
  port
  (
    -- Pixel reset and clock
    Pixel_Reset : in std_logic;
    Pixel_Clock : in std_logic;

    -- Declared input resolution
    --
    -- This input must be stable before the first valid input cycle after
    -- synchronous reset for there to be defined output behavior.
    Input_Resolution : in CL_Video_Coordinate;

    -- Input pixel stream control
    Input_Valid : in std_logic;
    Input_Start : in std_logic;
    
    -- Output coordinate and replication interface
    Coord_Valid     : out std_logic;
    Coordinate      : out CL_Video_Coordinate;
    Vert_First      : out std_logic;
    Vert_Last       : out std_logic;
    Horiz_First     : out std_logic;
    Horiz_Last      : out std_logic;
    Replicate_Row   : out std_logic;
    Replicate_Left  : out std_logic;
    Replicate_Right : out std_logic
  );
end entity;


-- Architecture implementation
architecture rtl of Bayer_Demosaic_Coords is

  -- Signals for locating the coordinate of each output cycle
  signal output_Coord       : CL_Video_Coordinate;
  signal output_X_tc        : std_logic;
  signal output_Y_tc        : std_logic;
  signal replicate_Row_int  : std_logic;
  signal flip_Replicate_Row : std_logic;
    
begin
  
  --
  -- Output coordinate location
  --
  -- Several state variables are updated which govern the multiplexing
  -- of pixel lanes to processing arithmetic, interpolation mode, etc.
  --
  coord_State : process(Pixel_Reset, Pixel_Clock)
    constant ZERO_OFFSET : CL_Video_Offset     := To_Unsigned(0, CL_Video_Offset'LENGTH);
    constant ZERO_COORD  : CL_Video_Coordinate := (x => ZERO_OFFSET,
                                                   y => ZERO_OFFSET);
  begin
    if ?? Pixel_Reset then
      Coord_Valid       <= '0';
      output_Coord      <= ZERO_COORD;
      Vert_First        <= '0';
      output_Y_tc       <= '0';
      Horiz_First       <= '0';
      output_X_tc       <= '0';
      replicate_Row_int <= '0';
      Replicate_Left    <= '0';
      Replicate_Right   <= '0';
    elsif rising_edge(Pixel_Clock) then

      -- Provide a systolic delay for the valid signal
      Coord_Valid <= Input_Valid;

      -- Clock-enable the first stage
      if ?? Input_Valid then
        -- Synchronously reset the algorithm state on the start cycle
        if ?? Input_Start then
          -- The output coordinate is reset to the origin pixel; no provision is
          -- made for image resolution down to a single chunk and / or line, so
          -- the terminal counts are also synchronously reset.
          output_Coord <= ZERO_COORD;
          Vert_First   <= '1';
          Horiz_First  <= '1';
          output_X_tc  <= '0';
          output_Y_tc  <= '0';

          -- The first cycle will perform nearest-neighbor replication for its
          -- first two pixels / the first row
          flip_Replicate_Row <= '0';
          replicate_Row_int  <= '1';
          Replicate_Left     <= '1';
          Replicate_Right    <= '0';
        else
          --
          -- This is not the start of a payload; update the coordinate state
          --
          
          -- Advance the output coordinate within the plane resolution limits
          if ?? output_X_tc then
            -- Reset to the left edge of the frame
            output_Coord.x <= ZERO_OFFSET;
            Vert_First     <= '0';
            Horiz_First    <= '1';
            output_X_tc    <= '0';
            
            -- Advance to the next line
            if not output_Y_tc then
              -- Synchronously detect terminal count
              output_Coord.y <= (output_Coord.y + 1);
              if(output_Coord.y = (Input_Resolution.y - 2)) then
                output_Y_tc <= '1';
              end if;
            end if;

            -- Replicate the first and last two rows of the frame
            flip_Replicate_Row <= '0';
            if((output_Coord.y = 0) or (output_Coord.y = (Input_Resolution.y - 4))) then
              flip_Replicate_Row <= '1';
            end if;
            replicate_Row_int <= (replicate_Row_int xor flip_Replicate_Row);

            -- The left columns are reset to replicate
            Replicate_Left  <= '1';
            Replicate_Right <= '0';
          else
            -- Synchronously detect terminal count and right-side replication
            output_Coord.x <= (output_Coord.x + PIXELS_PER_CLOCK);
            Horiz_First    <= '0';
            Replicate_Left <= '0';
            if(output_Coord.x = (Input_Resolution.x - (2 * PIXELS_PER_CLOCK))) then
              output_X_tc     <= '1';
              Replicate_Right <= '1';
            end if;
          end if;
        end if;
      end if;
      
    end if;
  end process;


  --
  -- Derive outputs from internal states
  --
  Coordinate    <= output_Coord;
  Vert_Last     <= output_Y_Tc;
  Horiz_Last    <= output_X_Tc;
  Replicate_Row <= replicate_Row_int;
  
end architecture;
