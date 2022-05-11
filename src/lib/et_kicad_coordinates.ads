------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      KICAD SCHEMATIC COORDINATES                         --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
with ada.text_io;				use ada.text_io;
with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;

with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;

with et_string_processing;
with et_logging;				use et_logging;
with et_geometry;				use et_geometry;
with et_coordinates;			use et_coordinates;
use et_coordinates.pac_geometry_2;


package et_kicad_coordinates is
	
	schematic_file_name_length : constant positive := 100; -- includes extension
	package type_schematic_file_name is new generic_bounded_length (schematic_file_name_length); 

	function to_string (schematic : in type_schematic_file_name.bounded_string) return string;
	function to_schematic_file_name (file : in string) return type_schematic_file_name.bounded_string;
	
	-- The name of a submodule may have 100 characters which seems sufficient for now.
 	submodule_name_length_max : constant positive := 100;
	package type_submodule_name is new generic_bounded_length (submodule_name_length_max); 
	use type_submodule_name;

-- 	procedure check_submodule_name_length (name : in string);
-- 	-- Checks if the given submodule name is not longer than allowed.
	
	submodule_name_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set("-_"); 

	procedure check_submodule_name_characters (
		name		: in type_submodule_name.bounded_string;
		characters	: in character_set := submodule_name_characters);
	-- Checks for forbidden characters in submodule name.

	
    -- The location of a submodule within the design hierarchy is reflected by
    -- a list of submodule names like motor_driver/counter/supply
    -- The first item in this list is the name of the top level module.
    package type_path_to_submodule is new doubly_linked_lists (
        element_type => type_submodule_name.bounded_string);

	function to_string (
		path 		: in type_path_to_submodule.list;
		top_module	: in boolean := true) return string;
	-- Returns the given path as string with hierarchy_separator.
	-- If top_module = false, the name of the top module is omitted.

	
	--type type_position is new type_point with private;
	type type_position is private;

	function get_point (position : in type_position) return type_point;
	function get_x (position : in type_position) return type_distance;
	function get_y (position : in type_position) return type_distance;

	procedure set_point (
		position	: in out type_position;
		place		: in type_point);

	
	procedure rotate_point (
		position	: in out type_position;
		angle		: in type_rotation);


	procedure mirror_point (
		position	: in out type_position;
		axis		: in type_axis_2D);


	procedure move_point (
		position	: in out type_position;
		offset		: in type_distance_relative);

	
	procedure set (
		position	: in out type_position;
		axis		: in type_axis_2D;
		value		: in type_distance);

	
	function path (position : in type_position) return type_path_to_submodule.list;

	procedure set_path (position : in out type_position; path : in type_path_to_submodule.list);
	-- Sets the path in given position.
	
	
	function to_string (submodule : in type_submodule_name.bounded_string) return string;
	function to_submodule_name (submodule : in string) return type_submodule_name.bounded_string;

	
	type type_scope is (
		XY, -- only x an y pos.
		SHEET, 	-- coordinates sheet related
		MODULE); -- coordinates with the module in scope

	function to_string (
	-- Returns the given position as string. Scope specifies how much position is to
	-- be displayed. See type_scope comments.
		position	: in type_position;
		scope		: in type_scope := SHEET)
		return string;


	function sheet (position : in type_position) return type_sheet;

	function same_path_and_sheet (left, right : in type_position) return boolean;
	-- Returns true if the given coordinates have same path and sheet.
	
	procedure set_sheet (position : in out type_position; sheet : in type_sheet);
	-- Sets the sheet number in given position.

	
	private 
	
		--type type_position is new type_point with record
		type type_position is record
			point			: type_point;
			path            : type_path_to_submodule.list; 
			sheet_number	: type_sheet := type_sheet'first;
		end record;

		
end et_kicad_coordinates;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
