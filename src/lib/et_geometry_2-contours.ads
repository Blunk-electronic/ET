------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / CONTOURS                               --
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
with ada.containers.indefinite_doubly_linked_lists;


generic
	
package et_geometry_2.contours is
	
	-- A contour is a list of lines, arcs or a single circle that form a path.
	-- If a contour consist of just a single circle then no other 
	-- segments are allowed.
	-- On the other hand, a contour may consist of lines and arcs. In that
	-- case no circle is allowed:
	type type_segment_shape is (LINE, ARC);

	
	type type_segment (shape : type_segment_shape) is record
		case shape is
			when LINE	=> segment_line : type_line;
			when ARC	=> segment_arc  : type_arc;
		end case;
	end record;


	function to_string (
		segment	: in type_segment)
		return string;



	-- There is NO regulation on the winding of a contour path.
	-- It can be CW or CCW.
	package pac_segments is new indefinite_doubly_linked_lists (type_segment);
	use pac_segments;
	
	
	-- Iterates the segments. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		segments	: in pac_segments.list;
		process		: not null access procedure (position : in pac_segments.cursor);
		proceed		: not null access boolean);


	type type_contour_segments (circular : boolean := false) is record
		case circular is
			when TRUE	=> circle   : type_circle;
			when FALSE	=> segments : pac_segments.list;
		end case;
	end record;
	

	type type_contour is tagged record
		contour : type_contour_segments;
	end record;


	-- Reads the segments provided in a row of
	-- arguments in a form like:
	-- "line 0 0 line 160 0 line 160 80 line 0 80"
	-- or:
	-- arc 50 50 0 50 ccw (50/50 center, 0/50 start, counter-clock-wise)
	-- and builds a contour.
	-- 1. The end point of a segment must not be specified.
	--    It is deduced from the start point of the successor segment.
	-- 2. A circle can only be read if it is the only shape.
	--    Mixing a circle with lines and arcs is not allowed.
	--    There must be only one circle.
	--    Examples:
	--     - valid  : circle 9 4 10
	--     - invalid: circle 34 45 30 circle 9 4 10
	--     - invalid: line 0 0 circle 9 4 10
	function to_contour (
		segments : in string)
		return type_contour'class;

	
	
	-- Returns the segments of a contour in human readable form.
	-- The segments are output in the same order as the contour
	-- has been defined by the operator:
	function to_string (
		contour	: in type_contour)
		return string;

	
	function get_segment (
		contour	: in type_contour;
		point	: in type_point)
		return pac_segments.cursor;


	
	type type_neigboring_segments is record
		-- The segment before a vertex.
		-- This segment ENDS on the vertex:
		segment_1 : pac_segments.cursor;

		-- The segment after a vertex:
		-- This segment STARTS on the vertex:
		segment_2 : pac_segments.cursor;
	end record;

	function get_neigboring_segments (
		contour	: in type_contour;
		vertex	: in type_point)
		return type_neigboring_segments;


	-- Returns the distance from the given reference point to
	-- to the nearest point on the contour.
	-- The point may be inside or outside the contour.
	function get_shortest_distance (
		contour	: in type_contour;
		point	: in type_point)
		return type_float_internal_positive;



	-- Loads the given segments into the given contour.
	-- NOTE: Overwrites already existing segments in the contour.
	procedure load_segments (
		contour		: in out type_contour;
		segments	: in type_contour_segments);
	
	
	procedure delete_segments (
		contour	: in out type_contour);


	
	procedure append_segment (
		contour	: in out type_contour;
		segment	: in type_segment);
	

	procedure set_circle (
		contour	: in out type_contour;
		circle	: in type_circle'class);
	
		
	function get_segments (
		contour	: in type_contour)
		return type_contour_segments;
	

	-- Returns 1 if the contour consist of just a single circle.
	-- Returns the number of segments if the contour consist of lines
	-- and/or arcs:
	function get_segments_total (
		contour : in type_contour)
		return count_type;



	-- Transposes a contour in Y direction.
	-- Each point of each segment gets shifted by
	-- the formula new_y = offset - old_y:
	procedure transpose_contour (
		contour	: in out type_contour'class;
		offset	: in type_distance);


	-- Reads the segments provided in a row of
	-- arguments in a form like:
	-- "line 0 0 line 160 0 line 160 80 line 0 80"
	-- or:
	-- arc 50 50 0 50 ccw (50/50 center, 0/50 start, counter-clock-wise)
	-- and builds a contour.
	-- 1. The end point of a segment must not be specified.
	--    It is deduced from the start point of the successor segment.
	-- 2. A circle can only be read if it is the only shape.
	--    Mixing a circle with lines and arcs is not allowed.
	--    There must be only one circle.
	--    Examples:
	--     - valid  : circle 9 4 10
	--     - invalid: circle 34 45 30 circle 9 4 10
	--     - invalid: line 0 0 circle 9 4 10
	function to_contour (
		arguments : in type_fields_of_line)
		return type_contour'class;

	

	-- Returns the boundaries of the given contour.
	function get_boundaries (
		contour		: in type_contour;
		line_width	: in type_distance_positive)
		return type_boundaries;
	

	-- A contour must have a properly closed outline.
	-- The outline check returns a list of points (where the gaps are):
	package pac_contour_gaps is new doubly_linked_lists (type_point); 

	
	-- Returns the points where gaps of a contour begin:
	function to_string (
		gaps : in pac_contour_gaps.list)
		return string;
	

	-- The result of an outline check is a parameterized type:
	type type_contour_status (closed : boolean) is record
		case closed is
			when TRUE	=> null;
			when FALSE	=> gaps : pac_contour_gaps.list;
		end case;
	end record;
				
	-- Returns true if the given contour is properly closed.
	-- If there are gaps, a list of points is returned where the gaps are.
	-- The test iterates the segments of the contour and tests whether
	-- the end point of a segment matches the start point of the next segment.
	-- CS: Special threatment for circle segments: Since a circle does not have 
	-- start and end point, only the center point of the circle must be in 
	-- the chain of segments. 
	-- CS: Improvement required: It is sufficient if the circle
	-- touches one of the other segments (lines and arcs) to regard it as connected
	-- with the contour.
	function is_closed (
		contour	: in type_contour)
		return type_contour_status;



	-- Moves a contour by the given offset. 
	procedure move_by (
		contour	: in out type_contour;
		offset	: in type_distance_relative);
	

	-- Mirrors a contour along the given axis.
	procedure mirror (
		contour	: in out type_contour;
		axis	: in type_axis_2d);
	

	-- Rotates a contour about the origin by the given rotation.
	procedure rotate_by (
		contour		: in out type_contour;
		rotation	: in type_rotation);



	-- In order to get the status of a point relative to
	-- a contour we need this stuff:
	-- The general approach is:
	-- A ray that starts at point and travels in zero degees 
	-- may intersect the contour edges.
	-- The result of such a query is the type_point_to_contour_status
	-- that contains a status flag (inside/outside) and a list
	-- of x values where the ray intersects the contour. For completeness
	-- the original point where the probe line has started is also provided.
	-- This list provides the x values ordered according to their
	-- distance to the start point of the ray. Lowest value first.

	type type_intersected_segment (shape : type_shape := LINE) is record
		case shape is 
			when LINE	=> segment_line : type_line;
			when ARC	=> segment_arc : type_arc;
			when CIRCLE	=> segment_circle : type_circle;
		end case;
	end record;



	-- For finding the lower left corner of a contour this type
	-- is required. The lower left corner can be a point somewhere
	-- on the edge of the contour. In that case the point is REAL.
	-- If the point is outside the contour then the point is VIRTUAL.
	type type_lower_left_corner_status is (
		REAL,	-- the corner point is somewhere on the contour
		VIRTUAL	-- the corner point is outside the contour (where
				-- the lowest x and lowest y are)
		);

	
	-- When the lower left corner is to be found, then
	-- the result of such a search operation is formed by
	-- this type:
	type type_lower_left_corner is record
		point	: type_point := origin;
		status	: type_lower_left_corner_status := REAL;
	end record;

	

	-- Searches the lower left corner of a contour.
	-- This function uses the boundaries of the contour.
	-- So the lower left corner can also be virtual, means
	-- outside the contour:
	function get_lower_left_corner (
		contour	: in type_contour)
		return type_lower_left_corner;


	-- Returns the corner that is closest to the origin of the drawing:
	function get_corner_nearest_to_origin (
		contour	: in type_contour)
		return type_point;

	
	function is_vertex (
		contour	: in type_contour;
		point	: in type_point)
		return boolean;
	



	
private
					   
	function get_shortest_distance (
		contour	: in type_contour;
		point	: in type_vector)
		return type_float_internal;


	-- The location of a point relative to a contour:
	type type_location is (
		ON_EDGE,
		INSIDE,
		OUTSIDE,
		ON_VERTEX);
	
	function to_string (status : in type_location) return string;


	-- The intersection of a probe line with a segment of the contour can
	-- be described as:
	type type_probe_line_intersection_contour is record
		x_position	: type_float_internal;
		angle		: type_angle := 0.0;
		segment		: type_intersected_segment;
	end record;


	function "<" (left, right : in type_probe_line_intersection_contour)
		return boolean;


	package pac_probe_line_intersections_contour is new
		doubly_linked_lists (type_probe_line_intersection_contour);


	type type_point_to_contour_status (location : type_location) is record
		-- The point where the probe line has started:
		start			: type_point;

		-- The intersections of the probe line with the polygon edges:
		intersections	: pac_probe_line_intersections_contour.list;

		case location is
			when OUTSIDE | INSIDE =>
				-- The shortest distance of the start point (of the probe line)
				-- to the contour:
				distance : type_float_internal;
				
			when ON_EDGE =>
				edge : pac_segments.cursor;

			when ON_VERTEX =>
				edges : type_neigboring_segments;

		end case;
	end record;


	function to_string (
		i : in type_point_to_contour_status)
		return string;


	function get_point_to_contour_status (
		contour		: in type_contour;	
		point		: in type_point)
		return type_point_to_contour_status;

	
end et_geometry_2.contours;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
