-- File Name   : CL_Video_Defs.vhd
-- Author      : Eldridge M. Mount IV
-- Description : Package housing definitions and utilities for working with video
--               streams in logic conformant to the Critical Link standard for
--               video frame encapsulation.
-- 
--               Refer to document :
-- 
--               "Critical Link, LLC Frame Data Packing Design
--                (For Various Camera Systems)"
-- 
--               Revision DRAFT employed in the present incarnation of this module.
--
--     o  0
--     | /       Copyright (c) 2017
--    (CL)---o   Critical Link, LLC
--      \
--       O

-- IEEE standard packages
library ieee;
use ieee.numeric_std.all;


-- Package declaration
package CL_Video_Defs is

  --
  -- Constant definitions for header element location and slicing
  --

  -- Definition of a "field worth" of header data, to establish a basic
  -- unit of bit slicing and ordinal word counting.
  constant CL_VIDEO_HEADER_FIELD_BITS : positive := 16;

  -- Minimum number of header fields packed per clock cycle
  constant CL_VIDEO_MIN_FIELDS_PER_CLOCK : positive := 2;

  -- Field offsets within the common video stream header
  constant CL_VIDEO_FRAME_WIDTH_FIELD  : natural :=  0;
  constant CL_VIDEO_FRAME_HEIGHT_FIELD : natural :=  1;
  constant CL_VIDEO_ROI_OFFSET_X_FIELD : natural :=  2;
  constant CL_VIDEO_ROI_OFFSET_Y_FIELD : natural :=  3;
  constant CL_VIDEO_FRAME_INDEX_FIELD  : natural :=  4;
  constant CL_VIDEO_TIMESTAMP_FIELD    : natural :=  6;
  constant CL_VIDEO_SPARE_0_FIELD      : natural :=  8;
  constant CL_VIDEO_SPARE_1_FIELD      : natural := 10;
  constant CL_VIDEO_SPARE_2_FIELD      : natural := 12;
  constant CL_VIDEO_SPARE_3_FIELD      : natural := 14;

  -- Field offset at which the video payload begins
  constant CL_VIDEO_PAYLOAD_START_FIELD : natural := 16;

  -- Width of image coordinates, in bits
  constant CL_VIDEO_COORD_BITS : positive := 16;

  -- The stream width always is an integer-power-of-two multiple of the maximum
  -- pel depth; if fewer bits are specific for PEL_DEPTH for a core, pels are
  -- right-justtified and zero-padded. Any unused bit lanes optimize out during
  -- synthesis of a properly-designed pipeline.
  constant PEL_LANE_BITS : positive := 16;


  --
  -- Constants and types for the various field elements
  --

  -- Video image offsets / coordinate pairs
  subtype CL_Video_Offset is unsigned((CL_VIDEO_COORD_BITS - 1) downto 0);
  type CL_Video_Coordinate is record
    x : CL_Video_Offset;
    y : CL_Video_Offset;
  end record;

  -- Frame index field element
  constant CL_VIDEO_FRAME_INDEX_BITS : positive := 32;
  subtype CL_Video_Frame_Index is unsigned((CL_VIDEO_FRAME_INDEX_BITS - 1) downto 0);

  -- Timestamp field element
  constant CL_VIDEO_FRAME_TS_BITS : positive := 32;
  subtype CL_Video_Frame_Timestamp is unsigned((CL_VIDEO_FRAME_TS_BITS - 1) downto 0);

end package;
