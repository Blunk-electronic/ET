------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           TRACK SEGMENT                                  --
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


package body et_track_segment is


	procedure reset_line (
		line : in out type_track_line)
	is
		use et_design_rules_board;
	begin
		set_A (line, origin);
		set_B (line, origin);

		line.width := type_track_width'first;
		line.layer := type_signal_layer'first;
	end reset_line;




	function get_layer (
		line : in type_track_line)
		return type_signal_layer
	is (line.layer);




	function to_string (
		line	: in type_track_line;
		width	: in boolean)
		return string
	is
		l : type_track_line renames line;

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
		line_1, line_2	: in type_track_line;
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




	function in_layer_and_in_area (
		line	: in type_track_line;
		layer	: in type_signal_layer;
		area	: in type_area)
		return boolean
	is
		result : boolean := false;
	begin
		if get_layer (line) = layer and then
			in_area (line, area) then
				result := true;
		end if;

		return result;
	end in_layer_and_in_area;






-- ARCS:


	procedure reset_arc (
		arc : in out type_track_arc)
	is
		use et_directions;
		use et_design_rules_board;
	begin
		set_A (arc, origin);
		set_B (arc, origin);
		set_center (arc, origin);
		set_direction (arc, CCW);

		arc.width := type_track_width'first;
		arc.layer := type_signal_layer'first;
	end reset_arc;



	function get_layer (
		arc : in type_track_arc)
		return type_signal_layer
	is (arc.layer);



	function to_string (
		arc		: in type_track_arc;
		width	: in boolean)
		return string
	is
		a : type_track_arc renames arc;

		text : constant string := to_string (pac_geometry_2.type_arc (a))
				& "/ ly " & to_string (a.layer);
	begin
		if width then
			return text & "/ width " & to_string (a.width);
		else
			return text;
		end if;
	end to_string;




	function in_layer_and_in_area (
		arc		: in type_track_arc;
		layer	: in type_signal_layer;
		area	: in type_area)
		return boolean
	is
		result : boolean := false;
	begin
		if get_layer (arc) = layer and then
			in_area (arc, area) then
				result := true;
		end if;

		return result;
	end in_layer_and_in_area;









-- CIRCLES:

	function get_layer (
		circle : in type_track_circle)
		return type_signal_layer
	is (circle.layer);



	function to_string (
		circle	: in type_track_circle;
		width	: in boolean)
		return string
	is
		c : type_track_circle renames circle;

		text : constant string := to_string (pac_geometry_2.type_circle (c))
				& "/ ly " & to_string (c.layer);
	begin
		if width then
			return text & "/ width " & to_string (c.width);
		else
			return text;
		end if;
	end to_string;




	function in_layer_and_in_area (
		circle	: in type_track_circle;
		layer	: in type_signal_layer;
		area	: in type_area)
		return boolean
	is
		result : boolean := false;
	begin
		if get_layer (circle) = layer and then
			in_area (circle, area) then
				result := true;
		end if;

		return result;
	end in_layer_and_in_area;




end et_track_segment;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
