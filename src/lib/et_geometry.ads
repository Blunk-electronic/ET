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

with et_coordinates;

package et_geometry is

	type type_distance_point_from_line is record -- CS: should be private
		distance		: et_coordinates.type_distance := et_coordinates.zero_distance;
		sits_on_start	: boolean := false;
		sits_on_end		: boolean := false;
		out_of_range	: boolean := false;
	end record;

	type type_line_range is (
		inside_end_points,	-- start and end point excluded -- CS rename to between_end_points ?
		with_end_points,	-- start and end point included
		beyond_end_points	-- unlimited line assumed. extends beyond both start and end point into infinity
		);
	
	function distance_of_point_from_line (
	-- Computes the shortest distance (perpendicular) of a given point from the given line.
	-- If the point is outside the
	-- range of the x coordinate, the corresponding flag in the return value is set.
		point, line_start, line_end: in et_coordinates.type_point;
		line_range : in type_line_range) return type_distance_point_from_line;

	type type_axis is (X, Y, Z);
	
	generic
-- 		type type_point is private;
		type type_distance is delta <>;
	package geometry_operations_2d is
		zero : constant type_distance := 0.0;
		
		subtype type_axis_2d is type_axis range X .. Y;

		type type_point is tagged private;

		origin : constant type_point;

		function x (point : in type_point'class) return type_distance; -- CS class attr. not required ?
		function y (point : in type_point'class) return type_distance;		

		function set (x, y : in type_distance) return type_point'class;

		procedure set (
			axis 	: in type_axis_2d;
			value	: in type_distance;					 
			point	: in out type_point'class);

		
		procedure reset (point : in out type_point'class);
		-- Moves the given point to the origin (0/0).

		procedure move (
			point	: in out type_point'class;
			offset	: in type_point);

		
-- 		with function x (p : type_point) return type_distance;
-- 		with function y (p : type_point) return type_distance;

-- 		type type_distance_point_line is record
-- 			distance		: type_distance; -- CS := et_coordinates.zero_distance;
-- 			sits_on_start	: boolean := false;
-- 			sits_on_end		: boolean := false;
-- 			out_of_range	: boolean := false;
-- 		end record;
		
		
-- 		function distance (
-- 			point		: in type_point; 
-- 			line_start	: in type_point;
-- 			line_end 	: in type_point;
-- 			line_range	: in type_line_range) 
-- 			return type_distance_point_line;

		procedure dummy;
		
	private

		type type_point is tagged record
			x, y : type_distance := zero;
		end record;

		origin : constant type_point := (others => zero);
		
-- 		type type_distance_point_from_line2 is record
-- 			distance		: type_distance; -- CS := et_coordinates.zero_distance;
-- 			sits_on_start	: boolean;
-- 			sits_on_end		: boolean;
-- 			out_of_range	: boolean;
-- 		end record;


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

	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
