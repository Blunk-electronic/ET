------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             BOUNDING BOX                                 --
--                                                                          --
--                               B o d y                                    --
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




package body et_bounding_box is



	function get_bounding_box (
		circle	: in pac_circle.type_circle;
		width	: in type_distance_positive)
		return type_area
	is
		-- CS: Optimization required. Compiler options ?
		-- CS: output warning if radius is zero ?

		result : type_area;
		w : type_distance;

		d : constant type_distance := width / 2.0;
	begin
		w := 2.0 * (circle.radius + d);

		result.width := w;
		result.height := w;

		result.position.x := circle.place.x - w / 2.0;
		result.position.y := circle.place.y - w / 2.0;

		return result;
	end get_bounding_box;





	function get_bounding_box (
		arc		: in pac_arc.type_arc;
		width	: in type_distance_positive)
		return type_area
	is
		-- CS: Optimization required. Compiler options ?

		result : type_area;

		c : pac_circle.type_circle;
	begin
		c.place := arc.place;
		c.radius := type_distance_positive (get_radius_start (arc));

		result := get_bounding_box (c, width);

		return result;
	end get_bounding_box;





	function get_bounding_box (
		arc			: in pac_arc.type_arc;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation_2;
		mirror		: in type_mirror := MIRROR_NO)
		return type_area
	is
		-- Make a copy of the given arc:
		c : pac_arc.type_arc := arc;

		b : type_area;
	begin
		rotate_arc_by (c, rotation);

		-- Mirror the arc:
		case mirror is
			when MIRROR_NO => null;
			when MIRROR_ALONG_X_AXIS  => mirror_arc (c, MIRROR_ALONG_X_AXIS);
			when MIRROR_ALONG_Y_AXIS  => mirror_arc (c, MIRROR_ALONG_Y_AXIS);
		end case;

		-- Move the arc by offset_1:
		move_by (c, offset_1);

		-- Move the arc by offset_2:
		move_by (c, offset_2);

		-- Get the bounding-box of arc:
		b := get_bounding_box (c, width);
		-- put_line ("b " & to_string (b));

		return b;
	end get_bounding_box;





	function get_bounding_box (
		circle		: in pac_circle.type_circle;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation_2;
		mirror		: in type_mirror := MIRROR_NO)
		return type_area
	is
		-- Make a copy of the given circle:
		c : pac_circle.type_circle := circle;

		b : type_area;
		unused_result : type_area;
	begin
		-- Rotate the center of the circle:
		rotate_circle_by (c, rotation);

		-- Mirror the circle:
		case mirror is
			when MIRROR_NO => null;
			when MIRROR_ALONG_X_AXIS  => mirror_circle (c, MIRROR_ALONG_X_AXIS);
			when MIRROR_ALONG_Y_AXIS  => mirror_circle (c, MIRROR_ALONG_Y_AXIS);
		end case;

		-- Move the circle by offset_1:
		move_by (c, offset_1);

		-- Move the arc by offset_2:
		move_by (c, offset_2);

		-- Get the bounding-box of arc:
		b := get_bounding_box (c, width);
		-- put_line ("b " & to_string (b));

		return b;
	end get_bounding_box;



end et_bounding_box;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
