------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   MODULE CLIPBOARD / NET SEGMENTS                        --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
-- Mario Blunk / Blunk electronic                                           --
-- Buchfinkenweg 3 / 99097 Erfurt / Germany                                 --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--  To Do:
--
--


with et_module;
with et_generic_modules;			use et_generic_modules;

with et_sheets;						use et_sheets;
with et_schematic_coordinates;		use et_schematic_coordinates;
with et_schematic_geometry;			use et_schematic_geometry;
use et_schematic_geometry.pac_geometry_2;

with et_net_names;					use et_net_names;
with et_nets;						use et_nets;
with et_net_segment;				use et_net_segment;
with et_logging;					use et_logging;



package et_module_clipboard.net_segments is


-- COPY:
	
	-- This procedure copies a given net and net segment
	-- to the clipboard.
	-- 1. If the net does not exist in the clipboard yet,
	--    then it will be created there with the same properties
	--    as the given net (like scope or net class). Tracks and fill zones
	--    are not copied. Already existing strands and net segments
	--    are not copied.
	-- 2. If the net does exist, then it will not be created
	--    anew.
	-- 3. The given segment is added to the net.
	procedure copy_net_segment_to_clipboard (
		net_cursor		: in pac_nets.cursor;
		segment			: in type_net_segment;
		log_threshold	: in type_log_level);



	-- This procedure copies selected net segments to
	-- the clipboard:
	procedure copy_selected_net_segments_to_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		-- CS sheet : in type_sheet; -- to speed up the process ?
		log_threshold	: in type_log_level);





	

-- PASTE:

	-- Copies a net segment (along with its net labels and connectors)
	-- and inserts it in the given module on the given sheet.
	-- The net segment will be placed by the given offset.
	-- Creates the given net if not already existing:
	procedure paste_net_segment_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in type_net_name; -- RESET, MOTOR_ON_OFF
		segment			: in type_net_segment;
		sheet			: in type_sheet;
		offset			: in type_vector_model;
		log_threshold	: in type_log_level);

	

	-- This procedure copies net segments from the clipboard
	-- to the given module. The segments will be placed
	-- by the given offset:
	procedure paste_net_segments_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		offset			: in type_vector_model;
		log_threshold	: in type_log_level);


	
end et_module_clipboard.net_segments;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
