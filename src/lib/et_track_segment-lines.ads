------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        TRACK SEGMENT LINES                               --
--                                                                          --
--                              S p e c                                     --
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
--
-- DESCRIPTION:
--
--
--
-- history of changes:
--
--
--
-- To Do:
--
--
--

with ada.containers;			use ada.containers;

with ada.containers.doubly_linked_lists;

with et_board_geometry;			use et_board_geometry;
with et_pcb_signal_layers;		use et_pcb_signal_layers;
with et_conductor_segment;


package et_track_segment.lines is

	use pac_geometry_2;



	package pac_conductor_lines is new doubly_linked_lists (type_track_line);
	use pac_conductor_lines;


	function get_conductor_line (
		line_cursor : in pac_conductor_lines.cursor)
		return type_track_line;



	function get_A (
		line : in pac_conductor_lines.cursor)
		return type_vector_model;

	function get_B (
		line : in pac_conductor_lines.cursor)
		return type_vector_model;




	-- Returns the start/end point and layer as string.
	-- If "width" is true, then the segment width is also output:
	function to_string (
		line	: in pac_conductor_lines.cursor;
		width	: in boolean)
		return string;


	-- Returns the signal layer of the given line:
	function get_layer (
		line : in pac_conductor_lines.cursor)
		return type_signal_layer;



	-- Returns true if the status flag "proposed"
	-- of a conductor line is set:
	function is_proposed (
		line : in pac_conductor_lines.cursor)
		return boolean;


	-- Returns true if the status flag "selected"
	-- of a conductor line is set:
	function is_selected (
		line : in pac_conductor_lines.cursor)
		return boolean;



	-- Returns the length of a list
	-- of conductor lines:
	function get_length (
		lines	: in pac_conductor_lines.list)
		return string;

	-- CS do the same for arcs and circles




	-- Extracts those lines which are in the given layer:
	function get_lines_by_layer (
		lines	: in pac_conductor_lines.list;
		layer	: in type_signal_layer)
		return pac_conductor_lines.list;



	-- Iterates the segments. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		lines	: in pac_conductor_lines.list;
		process	: not null access procedure (position : in pac_conductor_lines.cursor);
		proceed	: not null access boolean);



	-- Returns true if the given point sits on the given line.
	-- CS: rename to in_catch_zone (catch_zone, layer, line)
	function on_segment (
		point		: in type_vector_model; -- x/y
		layer		: in type_signal_layer;
		line		: in pac_conductor_lines.cursor)
		return boolean;


end et_track_segment.lines;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
