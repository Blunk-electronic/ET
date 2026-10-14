------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            BASIC OBJECTS                                 --
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
--
--  To Do:
--
--


with ada.containers;				use ada.containers;
with ada.containers.doubly_linked_lists;



with et_axes;						use et_axes;
with et_geometry_2a;

with et_string_processing;			use et_string_processing;
with et_object_status;				use et_object_status;
with et_mirroring;					use et_mirroring;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_directions;					use et_directions;



generic
	with package pac_geometry is new et_geometry_2a (<>);


package et_basic_objects is
	use pac_geometry;


	type type_object is abstract tagged record
		place 	: type_vector_model;
		status	: type_object_status;
	end record;


	type type_circle_2 is new type_object with record
		radius : type_distance_positive := 0.0;
	end record;


	a : natural := 0;

end et_basic_objects;

