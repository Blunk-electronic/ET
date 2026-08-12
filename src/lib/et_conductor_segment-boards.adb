------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                     CONDUCTOR SEGMENT IN BOARD                           --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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

--   For correct displaying set tab width in your edtior to 4.

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


package body et_conductor_segment.boards is


	procedure reset_line (
		line : in out type_conductor_line)
	is begin
		set_A (line, origin);
		set_B (line, origin);

		line.width := type_track_width'first;
		line.layer := type_signal_layer'first;
	end reset_line;




	function get_layer (
		line : in type_conductor_line)
		return type_signal_layer
	is (line.layer);




	function to_string (
		line	: in type_conductor_line;
		width	: in boolean)
		return string
	is
		l : type_conductor_line renames line;

		text : constant string := to_string (pac_geometry_2.type_line (l))
				& "/ ly " & to_string (l.layer);
	begin
		if width then
			return text & "/ width " & to_string (l.width);
		else
			return text;
		end if;
	end to_string;




	function are_connected (
		line_1, line_2	: in type_conductor_line;
		observe_layer	: in boolean := true)
		return boolean
	is
		result : boolean := false;
	begin
		-- test layers:
		if observe_layer then
			if line_1.layer /= line_2.layer then
				return false;
			end if;
		end if;

		-- test start and end points:
		if get_A (line_1) = get_A (line_2)
		or get_A (line_1) = get_B (line_2)
		or get_B (line_1) = get_A (line_2)
		or get_B (line_1) = get_B (line_2)
		then
			result := true;
		else
			result := false;
		end if;

		-- test start/end points between start/end points:
		if result = false then
			if line_1.on_line (to_vector (get_A (line_2)))
			or line_1.on_line (to_vector (get_B (line_2)))
			or line_2.on_line (to_vector (get_A (line_1)))
			or line_2.on_line (to_vector (get_B (line_1)))
			then
				result := true;
			end if;
		end if;

		return result;
	end are_connected;




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
			line : type_conductor_line renames element (c);
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




-- ARCS:


	procedure reset_arc (
		arc : in out type_conductor_arc)
	is
		use et_directions;
	begin
		set_A (arc, origin);
		set_B (arc, origin);
		set_center (arc, origin);
		set_direction (arc, CCW);

		arc.width := type_track_width'first;
		arc.layer := type_signal_layer'first;
	end reset_arc;



	function get_layer (
		arc : in type_conductor_arc)
		return type_signal_layer
	is (arc.layer);



	function to_string (
		arc		: in type_conductor_arc;
		width	: in boolean)
		return string
	is
		a : type_conductor_arc renames arc;

		text : constant string := to_string (pac_geometry_2.type_arc (a))
				& "/ ly " & to_string (a.layer);
	begin
		if width then
			return text & "/ width " & to_string (a.width);
		else
			return text;
		end if;
	end to_string;



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
			arc : type_conductor_arc renames element (c);
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





-- CIRCLES:

	function get_layer (
		circle : in type_conductor_circle)
		return type_signal_layer
	is (circle.layer);



	function to_string (
		circle	: in type_conductor_circle;
		width	: in boolean)
		return string
	is
		c : type_conductor_circle renames circle;

		text : constant string := to_string (pac_geometry_2.type_circle (c))
				& "/ ly " & to_string (c.layer);
	begin
		if width then
			return text & "/ width " & to_string (c.width);
		else
			return text;
		end if;
	end to_string;




	procedure iterate (
		circles	: in pac_conductor_circles.list;
		process	: not null access procedure (position : in pac_conductor_circles.cursor);
		proceed	: not null access boolean)
	is
		c : pac_conductor_circles.cursor := circles.first;
	begin
		while c /= pac_conductor_circles.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



	function get_layer (
		circle : in pac_conductor_circles.cursor)
		return type_signal_layer
	is (element (circle).layer);



	function is_proposed (
		circle : in pac_conductor_circles.cursor)
		return boolean
	is (is_proposed (element (circle)));




	function is_selected (
		circle : in pac_conductor_circles.cursor)
		return boolean
	is (is_selected (element (circle)));



end et_conductor_segment.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
