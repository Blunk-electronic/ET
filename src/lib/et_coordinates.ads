------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET COORDINATES                          --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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

--   The two letters "CS" indicate a "construction side" where things are not
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
with et_general;

package et_coordinates is

	type type_axis is (X, Y);

	-- There are drawings with the origin at the upper left corner (used by KiCad, ...).
	-- There are also drawings the the origin at the lower left corner (used by EAGLE, ...)
	type type_Y_axis_positive is (UPWARDS, DOWNWARDS);

	-- This flag is set on import or export of designs in accordance to the desired CAD format.
	-- Geometric operations like rotating require this flag.
	Y_axis_positive : type_Y_axis_positive := UPWARDS;
	
	-- The total distance between two objects:
	--	type type_distance is digits 9 range -100_000_000.0 .. 100_000_000.0; -- unit is metric millimeter
	type type_distance is delta 0.01 range -100_000_000.00 .. 100_000_000.00;
	for type_distance'small use 0.01; -- this is the accuracy required for schematic

	-- The x and y position of an object:
	subtype type_distance_xy is type_distance range -10_000_000.0 .. 10_000_000.0; -- unit is metric millimeter
	zero_distance : constant type_distance := 0.0;


	-- CS: type_grid ?

	mil_min : constant float := -390_000_000.0; -- equals approx. type_distance_xy'first
	mil_max : constant float :=  390_000_000.0; -- equals approx. type_distance_xy'last
	
	function mil_to_distance (mil : in string; warn_on_negative : boolean := true) 
		return type_distance_xy;
	-- Returns the given mils to type_distance_xy.

	function to_mil_string (distance : in type_distance_xy) return string;
	-- Returns the given distance as string in mil.
	
	function to_string (distance : in type_distance) return string;
	-- Returns the given distance as a string.


	units_per_cycle : constant float := 360.0;
	
	-- Angles are to be interpreted as: 
	-- positive angle -> counter clock wise
	-- negative angle -> clock wise
	type type_angle is delta 0.1 digits 4 range -359.9 .. 359.9; -- unit is degrees
	-- CS: a type that allows angles of multiples of 45 degrees ? 
	-- or check angle via separate function when required ?
	
	subtype type_angle_90 is type_angle range 0.0 .. 90.0;
	-- CS: make use of this type by membership tests when required

	function to_string (angle : in type_angle) return string;
	-- Returns the given angle as string. 

	procedure warning_angle_greater_90_degrees;


	
	
	type type_2d_point is tagged private;

	zero : constant type_2d_point;

	axis_separator		: constant string (1..1) := "/";
	--position_preamble	: constant string (1..15) := "position (x" & axis_separator & "y) ";
	position_preamble	: constant string (1..11) := " pos (x" & axis_separator & "y) ";
	--position_preamble	: constant string (1..6) := "(x" & axis_separator & "y) ";
	
	function to_string (point : in type_2d_point) return string;
	-- Returns the given point coordinates to a string.

	function distance_x (point : in type_2d_point) return type_distance_xy;
	-- Returns the x distance of point from the drawing origin.
	
	function distance_y (point : in type_2d_point) return type_distance_xy;
	-- Returns the y distance of point from the drawing origin.
	
	procedure set_x (point : in out type_2d_point; x : in type_distance_xy);
	-- Assigns a point the given x position.
	
	procedure set_y (point : in out type_2d_point; y : in type_distance_xy);
	-- Assigns a point the given y position. 
	
	procedure set_xy (
		point	 : in out type_2d_point'class;
		position : in type_2d_point'class);
	
	procedure mirror (
		point	: in out type_2d_point;
		axis	: in type_axis);	

	procedure move (
		point	: in out type_2d_point;
		offset	: in type_2d_point);

	procedure rotate (
	-- Rotates the given point by the given angle with the origin as center.
		point	: in out type_2d_point;
		angle	: in type_angle;
		log_threshold : in et_string_processing.type_log_level);

	function distance (point_1, point_2 : in type_2d_point) return type_distance;
	-- Returns the distance between the given points.

	schematic_file_name_length : constant positive := 100; -- includes extension
	package type_schematic_file_name is new generic_bounded_length (schematic_file_name_length); 

	function to_string (schematic : in type_schematic_file_name.bounded_string) return string;
	function to_schematic_file_name (file : in string) return type_schematic_file_name.bounded_string;
	
	-- The name of a submodule may have 100 characters which seems sufficient for now.
 	submodule_name_length_max : constant positive := 100;
	package type_submodule_name is new generic_bounded_length (submodule_name_length_max); use type_submodule_name;
	submodule_name_characters : character_set := to_set 
		(ranges => (('a','z'),('A','Z'),('0','9'))) or to_set("-_"); 

	procedure check_submodule_name_length (name : in string);
	-- Checks if the given submodule name is not longer than allowed.

	procedure check_submodule_name_characters (
		name : in type_submodule_name.bounded_string;
		characters : in character_set := submodule_name_characters);
	-- Checks for forbiddedn characters in submodule name.
	
	function to_string (submodule : in type_submodule_name.bounded_string) return string;
	-- Returns the given submodule name as string.

	function to_submodule_name (submodule : in string) return type_submodule_name.bounded_string;
	-- Converts a string to type_submodule_name.
	
	-- Instead of full submodule names, abbrevations like "MCU" or "MOT" are used:
	submodule_abbrevation_characters : character_set := to_set (span => ('A','Z')); 
	submodule_abbrevation_length_max : constant positive := 5;
	package type_submodule_abbrevation is new generic_bounded_length (submodule_abbrevation_length_max);
	procedure check_submodule_abbrevation_length (abbrevation : in string);
	-- Checks if the given submodule abbrevation is not longer than allowed.

	procedure check_submodule_abbrevation_characters (
	abbrevation : in type_submodule_abbrevation.bounded_string;
	characters : in character_set := submodule_abbrevation_characters);
	-- Checks for forbidden characters in submodule abbrevation.
	
	function to_string (abbrevation : in type_submodule_abbrevation.bounded_string) return string;
	-- Returns the given submodule abbrevation as string.

	function to_abbrevation (abbrevation : in string) return type_submodule_abbrevation.bounded_string;
	-- Converts a string to type_submodule_abbrevation.
	
	-- A submodule can be instantiated multiples times.
	-- CS: currently we limit the number of instances to this value. increase if neccessary.
	submodule_instances_max : constant positive := 10; 
	subtype type_submodule_instance is positive range 1..submodule_instances_max;

	procedure check_number_of_instances (instances : in string);
	-- Checks if given instances is a digit and if it is within allowed range.

	function to_number_of_instances (instances : in string) return type_submodule_instance;
	
	function to_string (instance : in type_submodule_instance) return string;
	-- Converts a submodule instance index to a string.

	function append_instance (
		submodule	: in type_submodule_name.bounded_string; -- nucleo_core
		separator	: in string := "_";
		instance	: in type_submodule_instance) -- 4
		return type_submodule_name.bounded_string; -- nucleo_core_4

	
    -- The location of a submodule within the design hierarchy is reflected by
    -- a list of submodule names like motor_driver/counter/supply
    -- The first item in this list is the name of the top level module.
    package type_path_to_submodule is new doubly_linked_lists (
        element_type => type_submodule_name.bounded_string);

	-- When handling hierachic structures we use a separator.
	-- Example: net name "HEATER_CONTROL/DRIVER/CLK"
	--hierarchy_separator : constant string (1..1) := ".";
	hierarchy_separator : constant string (1..1) := "/";

	-- A submodule may have up to x sheets.
	submodule_sheet_count_max : constant positive := 100; -- CS rename to frame_count_max
	type type_submodule_sheet_number is new positive range 1..submodule_sheet_count_max; -- CS rename to type_frame_number

	function to_string (sheet_number : in type_submodule_sheet_number) return string;
	-- Returns a sheet number to a string.

	function to_sheet_number (sheet_number : in string) return type_submodule_sheet_number;
	-- Converts a string to type_submodule_sheet_number
	
	-- The whole schematic may have a total of x pages.
	schematic_page_count_max : constant positive := 100;
	type type_schematic_page_number is new positive range 1..schematic_page_count_max; -- CS: not used yet
	
	function to_string (
		path : in type_path_to_submodule.list;
		top_module : in boolean := true) return string;
	-- Returns the given path as string with hierarchy_separator.
	-- If top_module = false, the name of the top module is omitted.
	
	type type_coordinates is new type_2d_point with private;

	function to_coordinates (point : in type_2d_point'class)
	-- Converts a type_2d_point to type_coordinates.
		return type_coordinates;

	zero_position : constant type_coordinates;

	coordinates_preamble_xy : constant string (1..11) := " pos "
		& "(x"
		& axis_separator
		& "y) ";
	
	coordinates_preamble_sheet : constant string (1..17) := " pos "
		& "(sheet"
		& axis_separator
		& "x"
		& axis_separator
		& "y) ";

	coordinates_preamble_module : constant string (1..22) := " pos "
		& "(path"
		& axis_separator
		& "sheet"
		& axis_separator
		& "x"
		& axis_separator
		& "y) ";

	
	type type_scope is (
		XY, -- only x an y pos.
		SHEET, 	-- coordinates sheet related
		MODULE); -- coordinates with the module in scope
		-- CS: rig ? -- with the whole rig is scope

	function to_string (
	-- Returns the given position as string. Scope specifies how much position is to
	-- be displayed. See type_scope comments.
		position	: in type_coordinates;
		scope		: in type_scope := SHEET)
		return string;

	function path (position : in type_coordinates) return type_path_to_submodule.list;
	
	function sheet (position : in type_coordinates) return type_submodule_sheet_number;

	function same_path_and_sheet (left, right : in type_coordinates) return boolean;
	-- Returns true if the given coordinates have same path and sheet.
	
	procedure set_path (position : in out type_coordinates; path : in type_path_to_submodule.list);
	-- Sets the path in given position.

	procedure set_sheet (position : in out type_coordinates; sheet : in type_submodule_sheet_number);
	-- Sets the sheet number in given position.

	-- PAPER SIZES
	-- As default we assume landscape format for all sheets.
	paper_size_A3_x : constant et_coordinates.type_distance := 420.0; -- CS use a common anchestor type and default value with sizes defined in et_pcb_coordinates.ads.
	paper_size_A3_y : constant et_coordinates.type_distance := 297.0;
	
	paper_size_A4_x : constant et_coordinates.type_distance := 297.0;
	paper_size_A4_y : constant et_coordinates.type_distance := 210.0;

	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
		paper_size	: in et_general.type_paper_size;
		orientation	: in et_general.type_paper_orientation := et_general.LANDSCAPE;
		axis		: in type_axis)
		return type_distance_xy;

	
	private 
		-- In general every object has at least x,y coordinates.
		type type_2d_point is tagged record
			x, y : type_distance_xy := zero_distance;
		end record;
		
		zero : constant type_2d_point := (x => zero_distance, y => zero_distance);

	
		type type_coordinates is new type_2d_point with record
			path            : type_path_to_submodule.list; -- CS: in native project not required.
			-- CS: A dedicated type_coordinates for kicad could make sense.
			
			sheet_number	: type_submodule_sheet_number := type_submodule_sheet_number'first;
		end record;

		zero_position : constant type_coordinates := (
			path			=> type_path_to_submodule.empty_list,
			sheet_number	=> type_submodule_sheet_number'first,
			x				=> 0.0,
			y				=> 0.0 );
		
end et_coordinates;

-- Soli Deo Gloria
