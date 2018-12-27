------------------------------------------------------------------------------
--                                                                          --
--                      SYSTEM ET PCB COORDINATES                           --
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

--   For correct displaying set tab width in your edtior to 4.

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
with ada.containers.indefinite_doubly_linked_lists;
with ada.containers.ordered_maps;
with ada.containers.indefinite_ordered_maps;
with ada.containers.ordered_sets;

with et_general;
with et_coordinates;
with et_string_processing;

package et_pcb_coordinates is
	
	type type_axis is (X, Y, Z);
	subtype type_axis_2d is type_axis range X .. Y; -- CS use this type for all kinds of 2d ops

	type type_face is (TOP, BOTTOM);

	function to_string (face : in type_face) return string;
	function to_face (face : in string) return type_face;
	
	-- The total distance between two objects:
	type type_distance_total is delta 0.001 range -100_000_000.00 .. 100_000_000.00; -- unit is metric millimeter
	for type_distance_total'small use 0.001; -- this is the accuracy required for layout

	function to_string (distance : in type_distance_total) return string;
	
	-- The x and y position of an object:
	subtype type_distance is type_distance_total range -10_000_000.0 .. 10_000_000.0; -- unit is metric millimeter
	zero_distance : constant type_distance := 0.0;

	-- PAPER SIZES
	-- As default we assume LANDSCAPE format for all sheets.
	paper_size_A3_x : constant type_distance := 420.0; -- CS use a common anchestor type and default value with sizes defined in et_coordinates.ads.
	paper_size_A3_y : constant type_distance := 297.0;
	
	paper_size_A4_x : constant type_distance := 297.0;
	paper_size_A4_y : constant type_distance := 210.0;

	function paper_dimension (
	-- Returns for the given paper size, orientation and axis the correspoinding size in mm.
		paper_size	: in et_general.type_paper_size;
		orientation	: in et_general.type_paper_orientation := et_general.LANDSCAPE;
		axis		: in type_axis_2d)
		return type_distance;
	
	-- PCB thickness (limited to reasonable range. CS adjust if required)
	pcb_thickness_min : constant type_distance := 0.1;
	pcb_thickness_max : constant type_distance := 20.0;	
	subtype type_pcb_thickness is type_distance_total range pcb_thickness_min .. pcb_thickness_max;
	
	mil_min : constant float := -390_000_000.0; -- equals approx. type_distance'first
	mil_max : constant float :=  390_000_000.0; -- equals approx. type_distance'last
	
	function mil_to_distance (mil : in string; warn_on_negative : boolean := true) 
		return type_distance;
	-- Converts a mil number (given as a string) to millimeters.	

	position_preamble_2d : constant string (1..10) := " pos "
		& "(x"
		& et_coordinates.axis_separator
		& "y)";

	
	position_preamble_3d : constant string (1..12) := " pos "
		& "(x"
		& et_coordinates.axis_separator
		& "y"
		& et_coordinates.axis_separator
		& "z)";
	
	function to_distance (distance : in string) return type_distance;

	units_per_cycle : constant float := 360.0;
	type type_angle is delta 0.01 range -359.9 .. 359.9;
	for type_angle'small use 0.01;
	zero_angle : constant type_angle := 0.0;

	function to_string (
		angle 		: in type_angle;
		preamble 	: in boolean := false)
		return string;

	function to_angle (angle : in string) return type_angle;

	type type_point_2d is tagged private;
	type type_point_3d is tagged private;

	function set_point (x, y : in type_distance) return type_point_2d'class;
	function set_point (x, y, z : in type_distance) return type_point_3d'class;
	
	function right_point_before_left_2d (right, left : in type_point_2d) return boolean;
	-- Returns true if right point comes before left point.
	-- Compares axis is this order: x, y
	-- If right point equals left point, returns false.

	type type_terminal_position is new type_point_2d with private;
	type type_submodule_position is new type_terminal_position with private;
	type type_package_position is new type_terminal_position with private;

	function to_string (point : in type_point_2d'class) return string;
	
	function terminal_position_default return type_terminal_position'class;

	function package_position_default return type_package_position;

	procedure reset_point (
	-- Moves the given point to the origin (0/0).
		point	: in out type_point_2d'class);					
	
	procedure move_point (
		point	: in out type_point_2d'class;
		offset	: in type_point_2d);

	procedure set_point (
		axis 	: in type_axis_2d;
		value	: in type_distance;					 
		point	: in out type_point_2d'class);

	procedure rotate (
	-- Rotates the given point by the given angle with the origin as center.
		point	: in out type_point_2d;
		angle	: in type_angle);
	
	function get_axis ( -- CS find a better name
		axis	: in type_axis_2d;
		point	: in type_point_2d)
		return type_distance_total;
	
	procedure set_angle (
		value	: in type_angle;
		point	: in out type_terminal_position'class);

	function get_angle (point : in type_terminal_position'class)
		return type_angle;

	procedure set_face (
		face	: in type_face;
		position: in out type_package_position);

	function get_face (packge : in type_package_position)
		return type_face;
	
	function to_terminal_position (
	-- Composes from a given point and angle the terminal position.
		point	: in type_point_2d;
		angle	: in type_angle)
		return type_terminal_position'class;

	
	private

		type type_point_2d is tagged record
			x, y : type_distance := zero_distance;
		end record;

		type type_point_3d is new type_point_2d with record
			z : type_distance := zero_distance;
		end record;

		zero_2d : constant type_point_2d := (others => zero_distance);
		zero_3d : constant type_point_3d := (others => zero_distance);

		type type_terminal_position is new type_point_2d with record
			angle	: type_angle := zero_angle;
		end record;

		type type_submodule_position is new type_terminal_position with null record;
		
		type type_package_position is new type_terminal_position with record
			face	: type_face := TOP;
		end record;

		
end et_pcb_coordinates;

-- Soli Deo Gloria
