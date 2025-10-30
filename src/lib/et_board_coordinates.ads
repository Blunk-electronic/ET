------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                           BOARD COORDINATES                              --
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

with et_board_geometry;				use et_board_geometry;
with et_coordinates_formatting;		use et_coordinates_formatting;
with et_string_processing;			use et_string_processing;

with et_pcb_sides;					use et_pcb_sides;


package et_board_coordinates is

	use pac_geometry_2;
	
	
	type type_package_position is new pac_geometry_2.type_position with private;

	

	package_position_default : constant type_package_position;
 


	-- This function returns the given package position
	-- as string formatted as follows:
	-- FORMAT_1 : x/y/rotation/face 4.5 / 5.6 / 90.0 / top
	-- FORMAT_2 : x 4.5 y 5.6 rotation 90.0 face top
	-- FORMAT_3 : 4.5 5.6 90.0 top
	overriding function to_string (
		position	: in type_package_position;
		format		: in type_output_format := FORMAT_1)
		return string;


	-- Reads a line like "position x 44.5 y 53.5 rotation 90.0 face top"
	-- starting at a field given by "from" and returns
	-- a package position:
	-- CS should be a procedure with an error flag output
	-- and the position output ?
	overriding function to_position (
		line : in type_fields_of_line;
		from : in type_field_count_positive)
		return type_package_position;

		
	function to_package_position (
		point 		: in type_vector_model;
		rotation	: in type_rotation_model := zero_rotation;
		face		: in type_face := TOP)
		return type_package_position;

	placeholder_position_default : constant type_package_position;	


	
	procedure set_face (
		position: in out type_package_position;
		face	: in type_face);

	
	function get_face (
		packge : in type_package_position)
		return type_face;


	-- Changes from top to bottom or vice versa:
	procedure toggle_face (
		position : in out type_package_position);
	
	
							
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
