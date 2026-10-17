------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             BOUNDING BOX                                 --
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

-- with ada.containers;			use ada.containers;
-- with ada.containers.doubly_linked_lists;
-- with ada.containers.indefinite_doubly_linked_lists;

-- with et_primitive_objects;		use et_primitive_objects;
with et_mirroring;					use et_mirroring;
with et_basic_objects;
with et_basic_objects.circle;
with et_basic_objects.arc;


generic

	with package pac_objects is new et_basic_objects (<>);
	with package pac_circle is new pac_objects.circle;
	with package pac_arc is new pac_objects.arc;

package et_bounding_box is
	use pac_objects;
	use pac_geometry;

	use pac_circle;
	use pac_arc;




	-- Returns the bounding-box of the given circle.
	-- It respects the linewidth of the circumfence:
	function get_bounding_box (
		circle	: in pac_circle.type_circle;
		width	: in type_distance_positive)
		return type_area;


	-- Returns the bounding-box of the given circle.
	-- It respects the linewidth of the circumfence.
	-- CS: For simlicity the given arc is threated like a circle.
	function get_bounding_box (
		arc		: in pac_arc.type_arc;
		width	: in type_distance_positive)
		return type_area;




	-- Computes the bounding-box of a given arc
	-- taking into account the given offsets, rotation
	-- and mirror style:
	function get_bounding_box (
		arc			: in pac_arc.type_arc;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation_2;
		mirror		: in type_mirror := MIRROR_NO)
		return type_area;


	-- Computes the bounding-box of a given circle
	-- taking into account the given offsets, rotation
	-- and mirror style:
	function get_bounding_box (
		circle		: in pac_circle.type_circle;
		width		: in type_distance_positive;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation_2;
		mirror		: in type_mirror := MIRROR_NO)
		return type_area;


end et_bounding_box;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
