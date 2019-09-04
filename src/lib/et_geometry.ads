------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY                                    --
--                                                                          --
--                               S p e c                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with et_general;				use et_general;

package et_geometry is

	generic
		type type_distance is delta <>;
		type type_rotation is delta <>;
		
	package geometry_operations_2d is
		zero : constant type_distance := 0.0;
		
		type type_point is tagged private;

		origin : constant type_point;

		function to_distance (distance : in string) return type_distance;		
		function to_string (distance : in type_distance) return string;
		
		function x (point : in type_point'class) return type_distance; -- CS class attr. not required ?
		function y (point : in type_point'class) return type_distance;		

		function mil_to_distance (mil : in string) return type_distance;
		-- Converts a mil number (given as a string) to millimeters.	

		function distance_to_mil (distance : in type_distance) return string;
		
		axis_separator : constant string := "/";
		point_preamble : constant string := " pos (x" & axis_separator & "y) ";
		point_preamble_with_rotation : constant string := " pos "
			& "(x"
			& axis_separator
			& "y"
			& axis_separator
			& "rotation)";

		function "<" (left, right : in type_point) return boolean;
		
		function set (x, y : in type_distance) return type_point'class;

		procedure set (
			axis 	: in type_axis_2d;
			value	: in type_distance;					 
			point	: in out type_point'class);

		procedure set (
			point	: in out type_point'class;
			position: in type_point);

		
		procedure reset (point : in out type_point'class);
		-- Moves the given point to the origin (0/0).

		procedure move (
			point	: in out type_point'class;
			offset	: in type_point);
		
		procedure mirror (
			point	: in out type_point;
			axis	: in type_axis_2d);	

		function distance (
		-- Returns the absolute distance on the given axis between the given points.
			point_1	: in type_point;
			point_2	: in type_point;
			axis	: in type_axis_2d) 
			return type_distance;
		
		function distance (point_one, point_two : in type_point) return type_distance;
		-- Computes the total distance between point_one and point_two.	

		type type_distance_point_line is record -- CS private ?
			distance		: type_distance := zero;
			sits_on_start	: boolean := false;
			sits_on_end		: boolean := false;
			out_of_range	: boolean := false;
		end record;

		type type_line_range is (
			INSIDE_END_POINTS,	-- start and end point excluded -- CS rename to between_end_points ?
			WITH_END_POINTS,	-- start and end point included
			BEYOND_END_POINTS	-- unlimited line assumed. extends beyond both start and end point into infinity
			);
		
		function distance_point_line (
			point		: in type_point; 
			line_start	: in type_point;
			line_end 	: in type_point;
			line_range	: in type_line_range) 
			return type_distance_point_line;

		function add (left, right : in type_rotation) return type_rotation;
		-- Adds two angles.
		-- If result greater or equal 360 degree then 360 degree is subtracted from result.
		-- If result less or equal 360 degree then 360 degree is added to the result.
		
		type type_position is new type_point with private;

-- 		function create (
-- 			point		: in type_point'class;
-- 			rotation	: in type_rotation) 
-- 			return type_position;
		
		function to_rotation (rotation : in string) return type_rotation;
		function to_string (rotation : in type_rotation) return string;
		
		origin_zero_rotation : constant type_position;
		
		units_per_cycle : constant float := 360.0;

		zero_rotation : constant type_rotation := 0.0;

		procedure set (
		-- Sets the rotation of a position. (position.rotation)
			position	: in out type_position;
			rotation	: in type_rotation);
		
		function rot (position : in type_position'class) return type_rotation;
		-- Returns the rotation of the given position.

		procedure rotate (
		-- Changes the rotation of the given position by the given offset.
		-- Preserves x/y. Changes position.rotation only.
			position	: in out type_position'class;
			offset		: in type_rotation);
		
		procedure rotate (
		-- Rotates the given point by the given angle around the origin.
		-- Changes point.x and point.y only.
			point		: in out type_point'class;
			rotation	: in type_rotation);
		
		function arc_end_point (
		-- Computes the end point of an arc.
			center		: in type_point;
			start_point	: in type_point;	
			angle 		: in type_rotation)
			return type_point'class;


		function to_string (point : in type_point) return string;
		function to_string (point : in type_position) return string;



		
	private

		type type_point is tagged record
			x, y : type_distance := zero;
		end record;

		origin : constant type_point := (others => zero);
		
		type type_position is new type_point with record
			rotation	: type_rotation := zero_rotation;
		end record;

		origin_zero_rotation : constant type_position := (others => <>);

	end geometry_operations_2d;

-- 	package geometry_operations_3d is
-- 		zero : constant type_distance := 0.0;
-- 		
-- 		subtype type_axis_3d is type_axis range X .. Z;
-- 
-- 		type type_point is tagged private;
-- 
-- 		origin : constant type_point;
-- 
-- 		function x (point : in type_point'class) return type_distance; -- CS class attr. not required ?
-- 		function y (point : in type_point'class) return type_distance;
-- 		function z (point : in type_point'class) return type_distance;				
-- 
-- 		function set (x, y, z : in type_distance) return type_point'class;
-- 
-- 		procedure dummy;
-- 		
-- 	private
-- 
-- 		type type_point is tagged record
-- 			x, y, z : type_distance := zero;
-- 		end record;
-- 
-- 		origin : constant type_point := (others => zero);
-- 		
-- 	end geometry_operations_3d;

	generic
		with package geometry is new geometry_operations_2d (<>);
		
	package shapes_2d is
		use geometry;

		-- LINE
		type type_line is abstract tagged record
			start_point 	: type_point;
			end_point   	: type_point;
			-- CS locked : type_locked;
		end record;

		-- A line is divided into three zones. Their width is the ratio
		-- of line length and the zone_division_factor.
		-- 
		--    S---|---center---|---E
		--
		-- The position of the bar (|) in this drawing depends on the zone_division_factor.
		-- The center length is twice the length of start/end point.
		type type_line_zone is (START_POINT, CENTER, END_POINT);
		line_zone_division_factor : constant positive := 4;
		
		function which_zone (
		-- Calculates the zone of the line where point is nearest.
			point	: in type_point'class;
			line	: in type_line'class) 
			return type_line_zone;
		
		-- ARC
		type type_arc is abstract tagged record
			center			: type_point;
			start_point		: type_point;
			end_point		: type_point;
			-- CS locked : type_locked;		
		end record;

		-- CIRCLE
		type type_circle is abstract tagged record
			center			: type_point;
			radius  		: type_distance := geometry.zero;
			-- CS locked : type_locked;
		end record;
		
		
	function to_string (line : in type_line) return string;
	-- Returns the start and end point of the given line as string.

	function to_string (arc : in type_arc) return string;
	-- Returns the start, end point and angle of the given arc as string.
	
	function to_string (circle : in type_circle) return string;
	-- Returns the center and radius of the given circle as string.
		
	end shapes_2d;
	
	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
