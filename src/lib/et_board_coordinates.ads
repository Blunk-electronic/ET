------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           BOARD COORDINATE                               --
--                                                                          --
--                               S p e c                                    --
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

with et_geometry_1;
with et_geometry_1.et_polygons;
with et_geometry_1.et_polygons.clipping;
with et_geometry_1.et_polygons.cropping;
with et_geometry_1.et_polygons.union;
with et_geometry_1.et_polygons.offsetting;

with et_geometry_2a;
with et_geometry_2a.contours;
with et_geometry_2a.grid;
with et_geometry_2a.path;

with et_pcb_sides;				use et_pcb_sides;


package et_board_coordinates is

	

	-- IMPORTANT: UNIT IS METRIC MILLIMETERS !!

	distance_digits_left  : constant :=  5;
	--distance_digits_right : constant := 10; -- 0.1pm
	distance_digits_right : constant := 4; -- 1um
	
	distance_smallest : constant := 1.0 / (10 ** distance_digits_right);
	
	type type_distance_model is delta distance_smallest 
		digits distance_digits_left + distance_digits_right
		range - 0.1 * (10 ** distance_digits_left) .. 
			  + 0.1 * (10 ** distance_digits_left);

	
		
	-- Angle or rotation is in mathematical sense, means:
	-- positive rotation -> counter clock wise
	-- negative rotation -> clock wise

	rotation_digits_left  : constant := 3;
	rotation_digits_right : constant := 7;

	rotation_smallest : constant := 1.0 / (10 ** rotation_digits_right);
	type type_rotation_model is delta rotation_smallest 
		digits rotation_digits_left + rotation_digits_right
		range -360.0 + rotation_smallest .. 360.0 - rotation_smallest;
		


	type type_float_model is digits 18;
	-- CS reduce digits. adapt accuracy
	-- when instantiating geometry package. See below.

	
	-- instantiation of the geometry 1 package:	
	package pac_geometry_brd is new et_geometry_1 (
		type_float	=> type_float_model,
 
		-- For assumed greatest numbers of 9999.999..
		-- we have 4 digits left and 14 digits right of comma.
		-- This leads to an accuracy of:
		accuracy	=> 1.0E-14
		-- CS: For numbers greater 9999.9 this accuracy is useless.
		);

	use pac_geometry_brd;


	

	package pac_polygons is new pac_geometry_brd.et_polygons;
	-- use pac_polygons;
	
	package pac_polygon_clipping is new pac_polygons.clipping;
	package pac_polygon_cropping is new pac_polygons.cropping;
	package pac_polygon_union is new pac_polygons.union;
	package pac_polygon_offsetting is new pac_polygons.offsetting;



	
	
	

	-- instantiation of the geometry_2 package:
	package pac_geometry_2 is new et_geometry_2a (
		pac_geometry_1			=> pac_geometry_brd,
		type_distance			=> type_distance_model,
		axis_max				=> +1_000.0,
		axis_min				=> -1_000.0,
		type_rotation			=> type_rotation_model
		);
	
	use pac_geometry_2;


	
-- FAB RELEVANT

	--fab_tolerance : constant type_distance_positive := 0.001;
	--fab_tolerance : constant type_distance_positive := 0.01;
	fill_tolerance : constant type_distance_positive := 0.05;

	

	

	-- In headless mode this accuracy should be used
	-- when locating objects inside a particual zone:
	accuracy_default : constant type_zone_radius := 2.0; 
	-- CS: should be a general setting for board and package editor in the future
	


	package pac_contours is new pac_geometry_2.contours;
	package pac_grid is new pac_geometry_2.grid;
	package pac_path_and_bend is new pac_geometry_2.path;
	

	
	type type_package_position is new pac_geometry_2.type_position with private;
	-- CS: Move into a separate package ! like et_package_position ?
	

	package_position_default : constant type_package_position;
 

	
	overriding function to_string (
		p : in type_package_position) 
		return string;

	
	function to_package_position (
		point 		: in type_vector_model;
		rotation	: in type_rotation_model := zero_rotation;
		face		: in type_face := TOP)
		return type_package_position;

	placeholder_position_default : constant type_package_position;	


	
	procedure set_face (
		face	: in type_face;
		position: in out type_package_position);

	
	function get_face (
		packge : in type_package_position)
		return type_face;


	procedure flip (
		position : in out type_package_position);
	
	
	-- Returns the location vector of the given
	-- package position:
	function get_place (
		position : in type_package_position)
		return type_vector_model;

	
	-- Returns the location vector and rotation of
	-- the given package position:
	function get_position (
		position : in type_package_position)
		return type_position;
							  
	
	-- Composes from a given point and angle the terminal position.
	function to_terminal_position (
		point		: in type_vector_model;
		rotation	: in type_rotation_model)
		return type_position'class;


	
	private
		
		type type_package_position is new pac_geometry_2.type_position 
			with record
			face : type_face := TOP;
		end record;

		package_position_default : constant type_package_position := (
			pac_geometry_2.origin_zero_rotation with face => TOP);

		placeholder_position_default : constant type_package_position := (
			pac_geometry_2.origin_zero_rotation with face => TOP);

		
end et_board_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
