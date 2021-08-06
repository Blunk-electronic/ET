------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            PCB COORDINATES                               --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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
	

	-- IMPORTANT: UNIT IS METRIC MILLIMETERS !!
	--type type_distance is delta 0.0001
	--type type_distance is new float
	distance_smallest : constant := 0.0001;
	type type_distance is delta distance_smallest digits 15
		range -10_000_000_000.00 .. 10_000_000_000.00; 

	distance_coarse_smallest : constant := type_distance'small * 10.0;
	type type_distance_coarse is delta distance_coarse_smallest 
		digits type_distance'digits - 1
		range type_distance'first .. type_distance'last;

		
	--for type_distance'small use 0.0001;
	--for type_distance'small use 0.0000001;
	-- CS increase accuracy if required


	
	-- Angle or rotation is in mathematical sense, means:
	-- positive rotation -> counter clock wise
	-- negative rotation -> clock wise
	
	--type type_rotation is delta 0.001 range -359.999 .. 359.999;
	--for type_rotation'small use 0.001;
	-- CS increase accuracy if required

	--rotation_smallest : constant float := float'small;
	rotation_smallest : constant := 0.001;
	--type type_rotation is new float 
	type type_rotation is delta rotation_smallest digits 6 
		range -360.0 + rotation_smallest .. 360.0 - rotation_smallest;
		
	
	
	
	-- instantiation of the geometry package:	
	package pac_geometry_brd is new et_geometry.generic_pac_geometry (
		type_distance			=> type_distance,
		type_distance_coarse	=> type_distance_coarse,
		axis_max				=> +10_000_000.0,
		axis_min				=> -10_000_000.0,
		type_rotation 			=> type_rotation);

	use pac_geometry_brd;


	
	-- PCB thickness (limited to reasonable range. CS adjust if required) -- CS move to design rules
	pcb_thickness_min : constant type_distance_positive := 0.1;
	pcb_thickness_max : constant type_distance_positive := 20.0;	
	subtype type_pcb_thickness is type_distance_positive 
		range pcb_thickness_min .. pcb_thickness_max;
	
	type type_package_position is new pac_geometry_brd.type_position with private;

	package_position_default : constant type_package_position;

	position_preamble : constant string := 
			" (x"
			& axis_separator
			& "y"
			& axis_separator
			& "rotation"
			& axis_separator
			& "face)";
	
	overriding function to_string (p : in type_package_position) return string;
	
	function to_package_position (
		point 		: in type_point;
		rotation	: in type_rotation := zero_rotation;
		face		: in type_face := TOP)
		return type_package_position;

	placeholder_position_default : constant type_package_position;	


	
	procedure set_face (
		face	: in type_face;
		position: in out type_package_position);

	function get_face (packge : in type_package_position)
		return type_face;
	
	function to_terminal_position (
	-- Composes from a given point and angle the terminal position.
		point		: in type_point;
		rotation	: in type_rotation)
		return type_position'class;


	
	private
		
		type type_package_position is new pac_geometry_brd.type_position with record
			face : type_face := TOP;
		end record;

		package_position_default : constant type_package_position := (
			pac_geometry_brd.origin_zero_rotation with face => TOP);

		placeholder_position_default : constant type_package_position := (
			pac_geometry_brd.origin_zero_rotation with face => TOP);

		
end et_pcb_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
