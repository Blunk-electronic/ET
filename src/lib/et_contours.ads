------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              CONTOURS                                    --
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

with ada.containers;				use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;

with et_object_status;				use et_object_status;
with et_primitive_objects;			use et_primitive_objects;

with et_basic_objects;
with et_basic_objects.circle;
with et_basic_objects.arc;


generic

	with package pac_objects is new et_basic_objects (<>);
	with package pac_circle is new pac_objects.circle;
	with package pac_arc is new pac_objects.arc;

package et_contours is
	use pac_objects;
	use pac_geometry;

	use pac_arc;
	-- use pac_circle;


-- SEGMENTS:

	-- A contour is a list of lines, arcs or a single circle that form a path.
	-- If a contour consist of just a single circle then no other
	-- segments are allowed.
	-- On the other hand, a contour may consist of lines and arcs. In that
	-- case no circle is allowed:
	type type_segment_shape is (LINE, ARC);
	-- CS rename to segment_shape_line, segment_shape_arc


	type type_segment (shape : type_segment_shape) is record -- CS rename to type_contour_segment
		case shape is
			when LINE	=> segment_line : type_line; -- CS rename segment_line to line
			when ARC	=> segment_arc  : pac_arc.type_arc;
		end case;
	end record;



	function to_string (
		segment	: in type_segment)
		return string;


	-- Converts a single line to a line segment:
	function to_segment (
		line : in type_line)
		return type_segment;


	-- CS: There should not be a regulation on the winding of a contour path.
	-- It can be CW or CCW.
	-- Currently it MUST be CCW !
	package pac_segments is new indefinite_doubly_linked_lists (type_segment);
	use pac_segments;


	function to_string (
		segment	: in pac_segments.cursor)
		return string;






	function is_proposed (
		segment	: in pac_segments.cursor)
		return boolean;


	function is_proposed (
		segment : in type_segment)
		return boolean;


	procedure set_proposed (
		segment : in out type_segment);


	procedure clear_proposed (
		segment : in out type_segment);


	function is_selected (
		segment	: in type_segment)
		return boolean;


	function is_selected (
		segment	: in pac_segments.cursor)
		return boolean;


	procedure set_selected (
		segment : in out type_segment);


	procedure clear_selected (
		segment : in out type_segment);



	function is_moving (
		segment	: in pac_segments.cursor)
		return boolean;


	procedure set_moving (
		segment : in out type_segment);


	procedure clear_moving (
		segment : in out type_segment);



	procedure modify_status (
		segment	: in out type_segment;
		operation	: in type_status_operation);


	procedure reset_status (
		segment	: in out type_segment);






	function get_shape (
		segment	: in type_segment)
		return type_segment_shape;


	function get_shape (
		segment	: in pac_segments.cursor)
		return type_segment_shape;



	-- Returns true if the given segment
	-- is in the given catch zone:
	function in_catch_zone (
		zone	: in type_catch_zone;
		segment : in type_segment)
		return boolean;


	-- Returns true if the given segment
	-- is in the given catch zone:
	function in_catch_zone (
		zone	: in type_catch_zone;
		segment : in pac_segments.cursor)
		return boolean;


end et_contours;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
