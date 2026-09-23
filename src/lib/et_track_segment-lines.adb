------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                        TRACK SEGMENT LINES                               --
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


package body et_track_segment.lines is



	function get_conductor_line (
		line_cursor : in pac_conductor_lines.cursor)
		return type_track_line
	is begin
		return element (line_cursor);
	end get_conductor_line;




	function get_A (
		line : in pac_conductor_lines.cursor)
		return type_vector_model
	is (get_A (element (line)));


	function get_B (
		line : in pac_conductor_lines.cursor)
		return type_vector_model
	is (get_B (element (line)));




	function to_string (
		line	: in pac_conductor_lines.cursor;
		width	: in boolean)
		return string
	is (to_string (element (line), width));




	function get_layer (
		line : in pac_conductor_lines.cursor)
		return type_signal_layer
	is (element (line).layer);





	function is_proposed (
		line : in pac_conductor_lines.cursor)
		return boolean
	is (is_proposed (element (line)));




	function is_selected (
		line : in pac_conductor_lines.cursor)
		return boolean
	is (is_selected (element (line)));




	function get_length (
		lines	: in pac_conductor_lines.list)
		return string
	is
		l : count_type;
	begin
		l := lines.length;
		return count_type'image (l);
	end get_length;




	function get_lines_by_layer (
		lines	: in pac_conductor_lines.list;
		layer	: in type_signal_layer)
		return pac_conductor_lines.list
	is
		result : pac_conductor_lines.list;

		procedure query_line (c : in pac_conductor_lines.cursor) is
			line : type_track_line renames element (c);
		begin
			if line.layer = layer then
				result.append (line);
			end if;
		end query_line;

	begin
		lines.iterate (query_line'access);
		return result;
	end get_lines_by_layer;



	procedure iterate (
		lines	: in pac_conductor_lines.list;
		process	: not null access procedure (position : in pac_conductor_lines.cursor);
		proceed	: not null access boolean)
	is
		c : pac_conductor_lines.cursor := lines.first;
	begin
		while c /= pac_conductor_lines.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



	function on_segment (
		point		: in type_vector_model; -- x/y
		layer		: in type_signal_layer;
		line		: in pac_conductor_lines.cursor)
		return boolean
	is
		result : boolean := false; -- to be returned
	begin
		if element (line).layer = layer then
			if element (line).on_line (to_vector (point)) then
				-- CS use
				--segment_line := to_line_segment (line);
				--distance := get_shortest_distance (point, segment_line);

				result := true;
			else
				result := false;
			end if;
		else
			result := false;
		end if;

		return result;
	end on_segment;


end et_track_segment.lines;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
