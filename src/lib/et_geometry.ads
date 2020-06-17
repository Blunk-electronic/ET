------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                              GEOMETRY                                    --
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
with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.numerics;

with glib;

with et_general;				use et_general;

package et_geometry is

	keyword_position	: constant string := "position";
	keyword_x 			: constant string := "x";
	keyword_y 			: constant string := "y";		
	keyword_rotation 	: constant string := "rotation";
	keyword_direction	: constant string := "direction";
	keyword_start		: constant string := "start";
	keyword_end			: constant string := "end";
	keyword_center		: constant string := "center";		
	keyword_radius		: constant string := "radius";		
	keyword_diameter	: constant string := "diameter";
	keyword_filled 		: constant string := "filled";

	type type_coordinates is (RELATIVE, ABSOLUTE);

	function to_string (coordinates : in type_coordinates) return string;
	function to_coordinates (coordinates : in string) return type_coordinates;

	type type_filled is (NO, YES);
	function to_string (filled : in type_filled) return string;
	function to_filled (filled : in string) return type_filled;
	filled_default : constant type_filled := NO;


	-- scale (relevant for GUI only):
	scale_min : constant glib.gdouble := 0.2;
	scale_max : constant glib.gdouble := 1000.0;
	subtype type_scale is glib.gdouble range scale_min .. scale_max;
	scale_default : constant type_scale := 1.0;
	scale_factor_on_zoom : constant type_scale := 1.05;

	
	generic
		type type_distance is delta <>;
		type type_rotation is delta <>;
		
	package generic_pac_geometry is
		
		zero : constant type_distance := 0.0;

		subtype type_distance_positive is type_distance range zero .. type_distance'last;
		subtype type_accuracy is type_distance range zero .. type_distance'last/1000;

		grid_max : constant type_distance_positive := type_distance_positive'last/1000;
		subtype type_distance_grid is type_distance_positive range zero .. grid_max;
		grid_default : constant type_distance_grid := 2.5;
		
		type type_grid is record
			x,y	: type_distance_grid := grid_default;
		end record;

		function to_string (grid : in type_grid) return string;
		
		type type_point is tagged private;

		origin : constant type_point;

		function to_string (point : in type_point) return string;
		
		function to_distance (distance : in string) return type_distance;		
		function to_string (distance : in type_distance) return string;
		
		function x (point : in type_point'class) return type_distance; -- CS class attr. not required ?
		function y (point : in type_point'class) return type_distance;		

		-- Returns the rotation of the given point around the origin.
		-- If for example point is (1/1) then the return is 45 degree.
		-- if point is (-1/-1) then the return is -135 degree.
		function rotation (point : in type_point) return type_rotation;

		
		-- The GUI frequently requires the area (a rectanglular box around an object)
		-- occupied by the object. For preparation we need the type_boundaries.
		-- Boundaries are to be set in the drawing plane where the y-axis increases upwards.
		-- The boundaries are always relative to a certain origin that
		-- sits somewhere inside the rectangle. 
		type type_boundaries is record
			smallest_x, smallest_y : type_distance := type_distance'last;
			greatest_x, greatest_y : type_distance := type_distance'first;
			distance_of_topleft_to_default : type_point := origin;
		end record;

		boundaries_default : constant type_boundaries;
		
		function to_string (boundaries : in type_boundaries) return string;
		
		-- Adds two boundaries.
		procedure add (
			boundaries_one : in out type_boundaries;
			boundaries_two : in type_boundaries);
		
		-- Calculates the boundaries of the given points:
		function boundaries (point_one, point_two : in type_point) return type_boundaries;

		-- Moves the boundaries by the given offset:
		procedure move_by (
			boundaries	: in out type_boundaries;
			offset		: in type_point);

		-- Rotates the given boundaries by given rotation.
		procedure rotate (
			boundaries	: in out type_boundaries;
			rotation	: in type_rotation);
		
		-- In the GUI, in connection with boundaries and bounding boxes a type for
		-- a rectangular area of the drawing is required.
		-- NOTE: The bounding box is something required in the model plane only.
		type type_rectangle is record
			x, y			: type_distance; -- position, upper left corner
			width, height	: type_distance_positive; -- size
		end record;

		no_rectangle : constant type_rectangle := (others => zero);

		function to_string (rectangle : in type_rectangle) return string;

		-- Moves the rectangle by the given offset:
		procedure move_by (
			rectangle	: in out type_rectangle;
			offset		: in type_point);
		
		-- Returns true if the given two rectangles intersect each other in some way:
		function intersects (rect1, rect2 : type_rectangle) return boolean;


		
		
		
		function mil_to_distance (mil : in string) return type_distance;
		-- Converts a mil number (given as a string) to millimeters.	

		function distance_to_mil (distance : in type_distance) return string;

		
		axis_separator : constant string := "/";
		point_preamble : constant string := " (x" & axis_separator & "y) ";
		point_preamble_with_rotation : constant string := 
			" (x"
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

		-- The quadrants of the coordinate system are numbered counter clockwise.
		-- Quadrant ONE is top right.
		type type_quadrant is (ONE, TWO, THREE, FOUR);

		function quadrant (point : in type_point) return type_quadrant;
		-- Returns the quadrant the point is located in.
		-- ONE  : point is right of the y-axis or on top of it AND above the x-axis or on top of it
		-- TWO  : point is left of the y-axis AND above the x-axis or on top of it
		-- THREE: point is left of the y-axis AND below the x-axis
		-- FOUR : point is right of the y-axis or on top of it AND below the x-axis
		
		function invert (point : in type_point'class) return type_point'class;
		-- Inverts the given point by multiplying x by -1 and y by -1.

		-- Inverts the point on the given axis.
		function invert (
			point	: in type_point;
			axis	: in type_axis_2d)
			return type_point'class;
		
		procedure reset (point : in out type_point'class);
		-- Moves the given point to the origin (0/0).

		procedure move_by (
		-- Moves a point by the given offset.
			point	: in out type_point'class;
			offset	: in type_point);

		procedure move_to (
		-- Moves a point to the given position.
			point		: in out type_point'class;
			position	: in type_point);
		
		function move (
		-- Moves a point into direction at distance.
			point		: in type_point;
			direction	: in type_rotation;
			distance	: in type_distance_positive)
			return type_point'class;
		
		procedure mirror (
		-- If axis is Y then it swaps right x with left x.
		-- If axis is X then it swaps upper y with lower y.
			point	: in out type_point;
			axis	: in type_axis_2d);	

		function distance (
		-- Returns the absolute distance on the given axis between the given points.
			point_1	: in type_point;
			point_2	: in type_point;
			axis	: in type_axis_2d) 
			return type_distance;

		function "+" (point_one, point_two : in type_point) return type_point'class;
		function "-" (point_one, point_two : in type_point) return type_point'class;
		
		function distance_relative (point_one, point_two : in type_point) return type_point'class;
		-- Returns the relative distance of point_two to point_one.	
		-- Subtracts point_one.x from point_two.y and point_one.y from point_two.y
		-- returns	d.x := point_two.x - point_one.x
		--			d.y := point_two.y - point_one.y;

		
		function distance_total (point_one, point_two : in type_point) return type_distance;
		-- Computes the total distance between point_one and point_two.

		function add (left, right : in type_rotation) return type_rotation;
		-- Adds two angles.
		-- If result greater 360 degree then 360 degree is subtracted from result.
		-- If result less than 360 degree then 360 degree is added to the result.

		type type_distance_polar is record -- CS private ?
			absolute: type_distance_positive;
			angle	: type_rotation;
		end record;

		function distance_polar (point_one, point_two : in type_point) return type_distance_polar;
		-- Returns the distance of point_two to point_one.	
		-- Subtracts point_one.x from point_two.y and point_one.y from point_two.y
		-- returns	total := sqrt ((point_two.x - point_one.x)**2 + (point_two.y - point_one.y)**2)
		--			angle := arctan ((point_two.y - point_one.y) / (point_two.x - point_one.x)

		
		type type_position is new type_point with private;

-- 		function create (
-- 			point		: in type_point'class;
-- 			rotation	: in type_rotation) 
-- 			return type_position;
		
		function to_rotation (rotation : in string) return type_rotation;
		function to_string (rotation : in type_rotation) return string;

		
		type type_direction_of_rotation is (
			CW,		-- clockwise
			CCW);	-- counterclockwise

		function to_string (direction : in type_direction_of_rotation) return string;
		function to_direction (direction : in string) return type_direction_of_rotation;

		function direction_of_rotation (rotation : in type_rotation) return type_direction_of_rotation;
		-- Returns CW if rotation is negative. Returns CCW if rotation is positive or zero.

		-- Changes CCW to CW and vice versa.
		function reverse_direction (direction : in type_direction_of_rotation)
			return type_direction_of_rotation;
		
		origin_zero_rotation : constant type_position;
		
		units_per_cycle : constant float := 360.0;

		zero_rotation : constant type_rotation := 0.0;

		radians_max : constant float := - 2.0 * ada.numerics.pi;
		
		subtype type_radians is float range (- radians_max) .. radians_max;
		
		function to_radians (degrees : in type_rotation) return float;
		-- Converts degrees to radians.

		function to_degrees (radians : in float) return type_rotation;
		-- Converts radians to degrees.

		
		
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
		
		procedure rotate_by (
		-- Rotates the given point BY the given angle around the origin.
		-- Changes point.x and point.y only.
			point		: in out type_point'class;
			rotation	: in type_rotation);

		procedure rotate_to (
		-- Rotates the given point TO the given angle around the origin.
		-- Changes point.x and point.y only.
			point		: in out type_point'class;
			rotation	: in type_rotation);

-- 		procedure rotate_by (
-- 		-- Rotates the given point BY the given angle around the given center point.
-- 		-- Changes point.x and point.y only.
-- 			point		: in out type_point'class;
-- 			center		: in type_point;
-- 			rotation	: in type_rotation);
		
		-- Returns a distance rounded according to given grid.		
		function round (
			distance	: in type_distance;
			grid		: in type_distance_grid)
			return type_distance;
		
		-- Returns x/y of point rounded according to given grid.
		function round_to_string (
			point	: in type_point;
			grid	: in type_grid)
			return string;

		-- Returns point rounded according to given grid.		
		-- Use this function for operations like "snap to grid" etc...
		function round (
			point	: in type_point;
			grid	: in type_grid)
			return type_point'class;

		overriding function to_string (point : in type_position) return string;



		
	private
		-- CS need an abstract type_point_abstract ?
		
		type type_point is tagged record
			x, y : type_distance := zero;
		end record;

		origin : constant type_point := (others => zero);
		
		type type_position is new type_point with record
			rotation : type_rotation := zero_rotation;
		end record;

		origin_zero_rotation : constant type_position := (others => <>);

		boundaries_default : constant type_boundaries := (others => <>);
		
	end generic_pac_geometry;




	generic
		with package pac_geometry is new generic_pac_geometry (<>);
		
	package generic_pac_shapes is
		use pac_geometry;

		-- Unites the point with the boundaries. boundaries is updated.
		procedure union (
			boundaries	: in out type_boundaries;
			point		: in type_point);
		
		-- Unites the boundaries "right" with boundaries "left". "left" is updated.
		procedure union (
			left	: in out type_boundaries;
			right	: in type_boundaries);
						
		
	-- LINE
		type type_line is abstract tagged record
			start_point 	: type_point;
			end_point   	: type_point;
			-- CS locked : type_locked;
		end record;

		procedure move_by (
		-- Moves a line by the given offset. 
			line	: in out type_line;
			offset	: in type_point);

		-- Mirrors a line along the given axis.
		procedure mirror (
			line		: in out type_line;
			axis		: in type_axis_2d);
		
		-- Rotates a line about the origin by the given rotation.
		procedure rotate_by (
			line		: in out type_line;
			rotation	: in type_rotation);


		
		function boundaries (line : in type_line) return type_boundaries;
		-- Returns the boundaries of the given line.
		
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

		type type_distance_point_line is record -- CS private ?
			distance		: type_distance_positive := zero;
			sits_on_start	: boolean := false;
			sits_on_end		: boolean := false;
			out_of_range	: boolean := false;
		end record;

		type type_line_range is (
			BETWEEN_END_POINTS,	-- start and end point excluded
			WITH_END_POINTS,	-- start and end point included
			BEYOND_END_POINTS	-- unlimited line assumed. extends beyond both start and end point into infinity
			);
		
		function distance_point_line (
		-- Computes the shortest distance (perpendicular) of a given point from the given line. If the point outside the
		-- range of the x coordinate, the corresponding flag in the return value is set.
			point		: in type_point; 
			line		: in type_line;
			line_range	: in type_line_range)
			return type_distance_point_line;

		function on_line (
		-- Returns true if the given point sits on the given line.
		-- The optional parameter accuracy may be used to specifiy the range at
		-- which the point is regarded as sitting on the line.
			point		: in type_point;
			line		: in type_line;
			accuracy	: in type_accuracy := zero)
			return boolean; 
		
	-- ARC
		type type_arc is abstract tagged record
			center			: type_point;
			start_point		: type_point;
			end_point		: type_point;
			direction		: type_direction_of_rotation := CW;
			-- CS locked : type_locked;		
		end record;

		-- Returns the distance between the start point and the center of the arc.
		function radius_start (arc : in type_arc) return type_distance_positive;

		-- Returns the distance between the end point and the center of the arc.
		function radius_end (arc : in type_arc) return type_distance_positive;

		-- Returns true if start and end point of given arc have same distance to center.
		function is_valid (arc : in type_arc) return boolean;
		
		-- Sometimes (for example with cairo) an arc must be
		-- expressed in terms of start and end angle:
		type type_arc_angles is record
			center		: type_point;
			radius		: type_distance_positive;
			angle_start	: type_rotation;
			angle_end	: type_rotation;
			direction	: type_direction_of_rotation := CW;
		end record;

		function to_arc_angles (arc : in type_arc) return type_arc_angles;
		-- Returns the start and end angles of an arc.
		
		function boundaries (arc : in type_arc) return type_boundaries;
		-- Returns the boundaries of the given arc.
		
		function on_arc (
		-- Returns true if the given point sits on the given arc.
		-- The optional parameter accuracy may be used to specifiy the range at
		-- which the point is regarded as sitting on the arc.
			point		: in type_point;
			arc			: in type_arc;
			accuracy	: in type_accuracy := zero)
			return boolean; 
		
		function arc_end_point (
		-- Computes the end point of an arc.
			center		: in type_point;
			start_point	: in type_point;	
			angle 		: in type_rotation) -- unit is degrees
			return type_point'class;

		procedure move_by (
		-- Moves an arc by the given offset. 
			arc		: in out type_arc'class;
			offset	: in type_point);

		procedure move_to (
		-- Moves an arc to the given position. 
			arc			: in out type_arc;
			position	: in type_point);

		procedure mirror (
		-- Mirrors an arc along the given axis.
			arc			: in out type_arc;
			axis		: in type_axis_2d);

		-- Rotates an arc about the origin by the given rotation.
		procedure rotate_by (
			arc			: in out type_arc;
			rotation	: in type_rotation);

		
	-- CIRCLE
		type type_circle is abstract tagged record -- CS rename to type_circle_base
			center			: type_point;
			radius  		: type_distance_positive := zero;
			-- CS locked : type_locked;
		end record;

		procedure move_by (
		-- Moves a circle by the given offset. 
			circle	: in out type_circle;
			offset	: in type_point);
		
		procedure mirror (
		-- Mirrors the center of a circle along the given axis.
			circle		: in out type_circle;
			axis		: in type_axis_2d);
		
		-- Rotates the center of a circle about the origin by the given rotation.
		procedure rotate_by (
			circle		: in out type_circle;
			rotation	: in type_rotation);
		
		function boundaries (circle : in type_circle) return type_boundaries;
		-- Returns the boundaries of the given circle.
		
		function on_circle (
		-- Returns true if the given point sits on the given circle circumfence.
		-- The optional parameter accuracy may be used to specifiy the range at
		-- which the point is regarded as sitting on the circle.
			point		: in type_point;
			circle		: in type_circle;
			accuracy	: in type_accuracy := zero)
			return boolean;
		
		
		function to_string (line : in type_line) return string;
		-- Returns the start and end point of the given line as string.

		function to_string (arc : in type_arc) return string;
		-- Returns the start, end point and angle of the given arc as string.
		
		function to_string (circle : in type_circle) return string;
		-- Returns the center and radius of the given circle as string.



		

	-- POLYGON

		type type_polygon_segment_count is new natural; -- CS range ?
		subtype type_polygon_segment_id is type_polygon_segment_count range 1 .. type_polygon_segment_count'last;
		
		type type_polygon_line is new type_line with record
			id : type_polygon_segment_id := type_polygon_segment_id'first;
		end record;
		
		package pac_polygon_lines is new doubly_linked_lists (type_polygon_line);

		type type_polygon_arc is new type_arc with record
			id : type_polygon_segment_id := type_polygon_segment_id'first;
		end record;
		
		package pac_polygon_arcs is new doubly_linked_lists (type_polygon_arc);

		type type_polygon_circle is new type_circle with record
			id : type_polygon_segment_id := type_polygon_segment_id'first;
		end record;
		
		package pac_polygon_circles is new doubly_linked_lists (type_polygon_circle);

		type type_polygon_segments is record
			lines	: pac_polygon_lines.list;
			arcs	: pac_polygon_arcs.list;
			circles	: pac_polygon_circles.list;
		end record;

		
		type type_polygon_base is abstract tagged record
			segments		: type_polygon_segments;
			segments_total	: type_polygon_segment_count := type_polygon_segment_count'first;
		end record;

		function boundaries (polygon : in type_polygon_base) return type_boundaries;
		-- Returns the boundaries of the given polygon.

		-- A polygon must have a properly closed outline.
		-- The outline check requires a list of points (where the gaps are):
		package pac_polygon_gaps is new doubly_linked_lists (type_point); 

		-- Returns the points where gaps of a polygon outline begin:
		function to_string (gaps : in pac_polygon_gaps.list) return string;
		

		-- The result of an outline check is a parameterized type:
		type type_polygon_status (closed : boolean) is record
			case closed is
				when TRUE	=> null;
				when FALSE	=> gaps : pac_polygon_gaps.list;
			end case;
		end record;
					
		-- Returns true if the given polygon is properly closed.
		-- If there are gaps, a list of points is returned where the gaps are.
		-- The test iterates the segments of the polygon and tests whether
		-- the end point of a segment matches the start point of the next segment.
		-- CS: Special threatment for circle segments: Since a circle does not have 
		-- start and end point, only the center point of the circle must be in 
		-- the chain of segments. 
		-- CS: Improvement required: It is sufficient if the circle
		-- touches one of the other segments (lines and arcs) to regard it as connected
		-- with the polygon.
		function is_closed (
			polygon	: in type_polygon_base)
			return type_polygon_status;

		procedure move_by (
		-- Moves a polygon by the given offset. 
			polygon	: in out type_polygon_base;
			offset	: in type_point);

		procedure mirror (
		-- Mirrors a polygon along the given axis.
			polygon	: in out type_polygon_base;
			axis	: in type_axis_2d);

		procedure rotate_by (
		-- Rotates a polygon about the origin by the given rotation.
			polygon		: in out type_polygon_base;
			rotation	: in type_rotation);


		-- POLYGON OFFSETTING:
		-- See:
		-- <https://gis.stackexchange.com/questions/61786/how-to-scale-reduce-my-polygon-without-changing-the-central-lat-long>
		-- <https://stackoverflow.com/questions/54033808/how-to-offset-polygon-edges>
		
		-- In order to do a polygon offset we have this type:
		type type_polygon_offset is (INWARD, OUTWARD);

		-- The procedure shrinks or expands the given polygon.
		procedure offset_polygon (
			polygon	: in out type_polygon_base;
			width	: in type_distance_positive;
			frame	: in type_polygon_offset);


		
		type type_polygon is new type_polygon_base with record
			filled	: type_filled;
		end record;

		
	end generic_pac_shapes;
	
	
end et_geometry;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
