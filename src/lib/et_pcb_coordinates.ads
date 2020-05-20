------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PCB COORDINATES                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
--                                                                          --
--    This program is free software: you can redistribute it and/or modify  --
--    it under the terms of the GNU General Public License as published by  --
--    the Free Software Foundation, either version 3 of the License, or     --
--    (at your option) any later version.                                   --
--                                                                          --
--    This program is distributed in the hope that it will be useful,       --
--    but WITHOUT ANY WARRANTY; without even the implied warranty of        --
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         --
--    GNU General Public License for more details.                          --
--                                                                          --
--    You should have received a copy of the GNU General Public License     --
--    along with this program.  If not, see <http://www.gnu.org/licenses/>. --
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
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;				use et_general;
with et_coordinates;
with et_string_processing;

with et_geometry;


package et_pcb_coordinates is

	keyword_face	: constant string := "face";	
	
	type type_face is (TOP, BOTTOM);

	face_default : constant type_face := TOP;
	function to_string (face : in type_face) return string;
	function to_face (face : in string) return type_face;

	-- Changes top to bottom and vice versa:
	procedure flip (face : in out type_face);
	
	-- The total distance between two objects:
	type type_distance_total is delta 0.001 range -100_000_000.00 .. 100_000_000.00; -- unit is metric millimeter
	for type_distance_total'small use 0.001; -- this is the accuracy required for layout

	-- The x and y position of an object:
	subtype type_distance is type_distance_total range -10_000_000.0 .. 10_000_000.0; -- unit is metric millimeter


	type type_rotation is delta 0.01 range -359.9 .. 359.9;
	for type_rotation'small use 0.01;
	
	-- instantiation of the 2d geometry package:	
	package geometry is new et_geometry.geometry_operations_2d (
		type_distance	=> type_distance_total,
		type_rotation 	=> type_rotation);
	
	
	-- PCB thickness (limited to reasonable range. CS adjust if required)
	pcb_thickness_min : constant geometry.type_distance_positive := 0.1;
	pcb_thickness_max : constant geometry.type_distance_positive := 20.0;	
	subtype type_pcb_thickness is type_distance_total range pcb_thickness_min .. pcb_thickness_max;
	
	type type_package_position is new geometry.type_position with private;

	package_position_default : constant type_package_position;
	placeholder_position_default : constant type_package_position;	

	procedure set_face (
		face	: in type_face;
		position: in out type_package_position);

	function get_face (packge : in type_package_position)
		return type_face;
	
	function to_terminal_position (
	-- Composes from a given point and angle the terminal position.
		point		: in geometry.type_point;
		rotation	: in type_rotation)
		return geometry.type_position'class;


	
	private
		
		type type_package_position is new geometry.type_position with record
			face : type_face := TOP;
		end record;

		package_position_default : constant type_package_position := (
			geometry.origin_zero_rotation with face => TOP);

		placeholder_position_default : constant type_package_position := (
			geometry.origin_zero_rotation with face => TOP);

		
end et_pcb_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
