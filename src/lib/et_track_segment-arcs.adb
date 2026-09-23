------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        TRACK SEGMENT ARCS                                --
--                                                                          --
--                              B o d y                                     --
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
--   to do:

-- with ada.text_io;			use ada.text_io;
with et_design_rules_board;
with et_directions;


package body et_track_segment.arcs is


	function get_A (
		arc : in pac_conductor_arcs.cursor)
		return type_vector_model
	is (get_A (element (arc)));


	function get_B (
		arc : in pac_conductor_arcs.cursor)
		return type_vector_model
	is (get_B (element (arc)));





	function to_string (
		arc		: in pac_conductor_arcs.cursor;
		width	: in boolean)
		return string
	is (to_string (element (arc), width));




	function get_layer (
		arc : in pac_conductor_arcs.cursor)
		return type_signal_layer
	is (element (arc).layer);



	function is_proposed (
		arc : in pac_conductor_arcs.cursor)
		return boolean
	is (is_proposed (element (arc)));




	function is_selected (
		arc : in pac_conductor_arcs.cursor)
		return boolean
	is (is_selected (element (arc)));





	function get_arcs_by_layer (
		arcs	: in pac_conductor_arcs.list;
		layer	: in type_signal_layer)
		return pac_conductor_arcs.list
	is
		result : pac_conductor_arcs.list;

		procedure query_arc (c : in pac_conductor_arcs.cursor) is
			arc : type_track_arc renames element (c);
		begin
			if arc.layer = layer then
				result.append (arc);
			end if;
		end query_arc;

	begin
		arcs.iterate (query_arc'access);
		return result;
	end get_arcs_by_layer;





	procedure iterate (
		arcs	: in pac_conductor_arcs.list;
		process	: not null access procedure (position : in pac_conductor_arcs.cursor);
		proceed	: not null access boolean)
	is
		c : pac_conductor_arcs.cursor := arcs.first;
	begin
		while c /= pac_conductor_arcs.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



	function on_segment (
		point		: in type_vector_model; -- x/y
		layer		: in type_signal_layer;
		arc			: in pac_conductor_arcs.cursor)
		return boolean
	is
		pragma unreferenced (point);
		result : boolean := false; -- to be returned
	begin
		if element (arc).layer = layer then
			-- CS use
			--segment_arc := to_arc_segment (arc);
			--distance := get_shortest_distance (point, segment_arc);

			result := true; -- CS
		else
			result := false;
		end if;

		return result;
	end on_segment;



end et_track_segment.arcs;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
