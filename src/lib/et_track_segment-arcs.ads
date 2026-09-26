------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        TRACK SEGMENT ARCS                                --
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


package et_track_segment.arcs is

	use pac_geometry_2;


	package pac_conductor_arcs is new doubly_linked_lists (type_track_arc);
	use pac_conductor_arcs;
	-- CS: rename to pac_track_arcs



	function get_conductor_arc (
		arc_cursor : in pac_conductor_arcs.cursor)
		return type_track_arc;


	function get_A (
		arc : in pac_conductor_arcs.cursor)
		return type_vector_model;


	function get_B (
		arc : in pac_conductor_arcs.cursor)
		return type_vector_model;



	-- Returns the start/end point, center and layer as string.
	-- If "width" is true, then the segment width is also output:
	function to_string (
		arc		: in pac_conductor_arcs.cursor;
		width	: in boolean)
		return string;



	-- Returns the signal layer of the given arc:
	function get_layer (
		arc : in pac_conductor_arcs.cursor)
		return type_signal_layer;


	-- Returns true if the status flag "proposed"
	-- of a conductor arc is set:
	function is_proposed (
		arc : in pac_conductor_arcs.cursor)
		return boolean;


	-- Returns true if the status flag "selected"
	-- of a conductor arc is set:
	function is_selected (
		arc : in pac_conductor_arcs.cursor)
		return boolean;





	-- Extracts those arcs which are in the given layer:
	function get_arcs_by_layer (
		arcs	: in pac_conductor_arcs.list;
		layer	: in type_signal_layer)
		return pac_conductor_arcs.list;




	-- Iterates the segments. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		arcs	: in pac_conductor_arcs.list;
		process	: not null access procedure (position : in pac_conductor_arcs.cursor);
		proceed	: not null access boolean);




	-- Returns true if the given point sits on the given arc.
	function on_segment (
		point		: in type_vector_model; -- x/y
		layer		: in type_signal_layer;
		arc			: in pac_conductor_arcs.cursor)
		return boolean;




end et_track_segment.arcs;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
