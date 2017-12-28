------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET COORDINATES                          --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   For correct displaying set tab with in your editor to 4.

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

-- with ada.strings.maps;			use ada.strings.maps;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.containers; 			use ada.containers;

with ada.containers.doubly_linked_lists;

with et_string_processing;

package et_coordinates is

	type type_axis is (X, Y);

	-- There are drawings with the origin at the upper left corner (used by KiCad, ...).
	-- There are also drawings the the origin at the lower left corner (used by EAGLE, ...)
	type type_Y_axis_positive is (upwards, downwards);

	-- This flag is set on import or export of designs in accordance to the desired CAD format.
	-- Geometric operations like rotating require this flag.
	Y_axis_positive : type_Y_axis_positive := upwards;
	
	-- The total distance between two objects:
	--	type type_distance is digits 9 range -100_000_000.0 .. 100_000_000.0; -- unit is metric millimeter
	type type_distance is delta 0.01 range -100_000_000.00 .. 100_000_000.00;
	for type_distance'small use 0.01; -- this is the accuracy required for schematic

	-- The x and y position of an object:
	subtype type_distance_xy is type_distance range -10_000_000.0 .. 10_000_000.0; -- unit is metric millimeter
	zero_distance : constant type_distance := 0.0;


	-- CS: type_grid ?

	function mil_to_distance (mil : in string; warn_on_negative : boolean := true) 
		return type_distance_xy;
	-- Returns the given mils to type_distance_xy.

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
	position_preamble	: constant string (1..11) := "pos. (x" & axis_separator & "y) ";
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
	
	procedure set (
		point	 : in out type_2d_point;
		position : in type_2d_point);
	
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
	
	-- The name of a submodule may have 100 characters which seems sufficient for now.
 	submodule_name_length : constant natural := 100;
	package type_submodule_name is new generic_bounded_length (submodule_name_length); use type_submodule_name;

	function to_string (submodule : in type_submodule_name.bounded_string) return string;
	-- Returns the given submodule name as string.
	
    -- The location of a submodule within the design hierarchy is reflected by
    -- a list of submodule names like motor_driver.counter.supply
    -- The first item in this list is the name of the top level module.
    package type_path_to_submodule is new doubly_linked_lists (
        element_type => type_submodule_name.bounded_string);

	-- When handling hierachic structures we use a separator.
	-- Example: net name "HEATER_CONTROL/DRIVER/CLK"
	--hierarchy_separator : constant string (1..1) := ".";
	hierarchy_separator : constant string (1..1) := "/";
	
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

	coordinates_preamble_xy : constant string (1..11) := "pos. "
		& "(x"
		& axis_separator
		& "y) ";
	
	coordinates_preamble_sheet : constant string (1..17) := "pos. "
		& "(sheet"
		& axis_separator
		& "x"
		& axis_separator
		& "y) ";

	coordinates_preamble_module : constant string (1..22) := "pos. "
		& "(path"
		& axis_separator
		& "sheet"
		& axis_separator
		& "x"
		& axis_separator
		& "y) ";
	
	type type_scope is (
		xy, -- only x an y pos.
		sheet, 	-- coordinates sheet related
		module); -- coordinates with the module in scope
		-- CS: rig ? -- with the whole rig is scope
	
	function to_string (
	-- Returns the given position as string. Scope specifies how much position is to
	-- be displayed. See type_scope comments.
		position	: in type_coordinates;
		scope		: in type_scope := sheet)
		return string;

	function path   (position : in type_coordinates) return type_path_to_submodule.list;
	function sheet  (position : in type_coordinates) return positive;

	function same_path_and_sheet (left, right : in type_coordinates) return boolean;
	-- Returns true if the given coordinates have same path and sheet.
	
	procedure set_path (position : in out type_coordinates; path : in type_path_to_submodule.list);
	-- Sets the path in given position.

	procedure set_sheet (position : in out type_coordinates; sheet : in positive);
	-- Sets the sheet number in given position.

	
	private 
		-- In general every object has at least x,y coordinates.
		type type_2d_point is tagged record
			x, y : type_distance_xy := zero_distance;
		end record;
		
		zero : constant type_2d_point := (x => zero_distance, y => zero_distance);

	
		type type_coordinates is new type_2d_point with record
			path            : type_path_to_submodule.list;
			sheet_number	: positive; -- CS: dedicated type
		end record;

		zero_position : constant type_coordinates := (
			path			=> type_path_to_submodule.empty_list,
			sheet_number	=> 1,
			x				=> 0.0,
			y				=> 0.0 );
		
end et_coordinates;

-- Soli Deo Gloria
