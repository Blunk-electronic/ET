------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                      GEOMETRY 2 / CONTOURS                               --
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

with ada.containers; 			use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.indefinite_doubly_linked_lists;


generic
	
package et_geometry_2a.contours is
	
	-- A contour is a list of lines, arcs or a single circle that form a path.
	-- If a contour consist of just a single circle then no other 
	-- segments are allowed.
	-- On the other hand, a contour may consist of lines and arcs. In that
	-- case no circle is allowed:
	type type_segment_shape is (LINE, ARC); -- CS rename to segment_shape_line, segment_shape_arc

	
	type type_segment (shape : type_segment_shape) is record -- CS rename to type_contour_segment
		case shape is
			when LINE	=> segment_line : type_line; -- CS rename segment_line to line
			when ARC	=> segment_arc  : type_arc;
		end case;
	end record;


	function to_string (
		segment	: in type_segment)
		return string;


	-- Converts a single line to a line segment:
	function to_segment (
		line : in type_line)
		return type_segment;

	

	-- CS: There should not be a regulation on the winding of a contour path.
	-- It can be CW or CCW.
	-- Currently it MUST be CCW !
	package pac_segments is new indefinite_doubly_linked_lists (type_segment);
	use pac_segments;
	

	function to_string (
		segment	: in pac_segments.cursor)
		return string;


	function is_proposed (
		segment	: in pac_segments.cursor)
		return boolean;


	function is_proposed (
		segment : in type_segment)
		return boolean;
	
		
	procedure set_proposed (
		segment : in out type_segment);
	

	procedure clear_proposed (
		segment : in out type_segment);


	function is_selected (
		segment	: in type_segment)
		return boolean;

	
	function is_selected (
		segment	: in pac_segments.cursor)
		return boolean;


	procedure set_selected (
		segment : in out type_segment);

	
	procedure clear_selected (
		segment : in out type_segment);



	function is_moving (
		segment	: in pac_segments.cursor)
		return boolean;


	procedure set_moving (
		segment : in out type_segment);

	
	procedure clear_moving (
		segment : in out type_segment);

	

	procedure modify_status (
		segment 	: in out type_segment;
		operation	: in type_status_operation);
	

	procedure reset_status (
		segment 	: in out type_segment);
							   


	function get_shape (
		segment	: in type_segment)
		return type_segment_shape;

	
	function get_shape (
		segment	: in pac_segments.cursor)
		return type_segment_shape;



	-- Returns true if the given segment
	-- is in the given catch zone:
	function in_catch_zone (
		zone	: in type_catch_zone;
		segment : in type_segment)
		return boolean;
	

	-- Returns true if the given segment
	-- is in the given catch zone:
	function in_catch_zone (
		zone	: in type_catch_zone;
		segment : in pac_segments.cursor)
		return boolean;


	-- Moves a segment according to the point of attack:
	procedure move_segment (
		segment			: in out type_segment;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model);
	
	
	-- Iterates the segments. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		segments	: in pac_segments.list;
		process		: not null access procedure (position : in pac_segments.cursor);
		proceed		: not null access boolean);


	-- A contour exists in two forms:
	-- 1. As a single circle.
	-- 2. As a list of lines and arcs:
	type type_segments (circular : boolean := false) is record
		case circular is
			when TRUE	=> circle   : type_circle;
			when FALSE	=> segments : pac_segments.list;
		end case;
	end record;
	

	type type_contour is tagged record
		contour : type_segments;
		-- CS status: moving, selected, locked, proposed ?
	end record;


	-- Returns true if the given contour consists of a circle:
	function is_circular (
		contour : in type_contour)
		return boolean;
	

	package pac_proposed_segments is new doubly_linked_lists (pac_segments.cursor);


	-- Returns the proposed segments of a contour:
	function get_proposed_segments (
		contour	: in type_contour)
		return pac_proposed_segments.list;


	

	
	-- In order to detect start and end points
	-- of a non-circular contour we need this type:
	type type_non_circular_vertex (circular : boolean) is record
		case circular is
			when FALSE	=> vertex : type_vector_model;
			when TRUE	=> null;
		end case;
	end record;

	
	-- Returns the first vertext (the start point) of
	-- a non-circular contour.
	-- If the given contour is circular then the result
	-- contains no vertex because a circle has no corners.
	-- This function does not care whether the contour is
	-- closed or not. It just returns the place where the contour
	-- begins:
	function get_A (
		contour : in type_contour)
		return type_non_circular_vertex;

	
	-- Returns the last vertext (the end point) of
	-- a non-circular contour.
	-- If the given contour is circular then the result
	-- contains no vertex because a circle has no corners.
	-- This function does not care whether the contour is
	-- closed or not. It just returns the place where the contour
	-- ends:
	function get_B (
		contour : in type_contour)
		return type_non_circular_vertex;



	
	

	-- Reads the segments provided in a row of
	-- arguments in a form like:
	-- "line 0 0 line 160 0 line 160 80 line 0 80"
	-- or:
	-- arc 50 50 0 50 ccw (50/50 center, 0/50 start, counter-clock-wise)
	-- and builds a contour.
	-- 1. The end point of a segment must not be specified.
	--    It is deduced from the start point of the successor segment.
	-- 2. If the contour consists of lines and/or arcs then 
	--    the end point of the last segment is where the contour
	--    has started. This implies that this procedure ALWAYS creates
	--    a CLOSED contour !
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


	

	
	-- Reads the segments provided in a row of
	-- arguments in a form like:
	-- "line 0 0 line 160 0 line 160 80 line 0 80"
	-- or:
	-- arc 50 50 0 50 ccw (50/50 center, 0/50 start, counter-clock-wise)
	-- and builds a contour.
	-- All characters after a comment character '#' will be ignored.
	-- See more details in function to_contour (see above).
	function to_contour (
		segments : in string)
		return type_contour'class;

	
	
	
	-- Returns the segments of a contour in human readable form.
	-- The segments are output in the same order as the contour
	-- has been defined by the operator.
	-- By default outputs the start points 
	-- only (for arcs in addition the center).
	-- Because the end point of a segment is usually the start point of the
	-- next segment.
	-- If full is true, then end points of segments are output also:
	function to_string (
		contour	: in type_contour;
		full	: in boolean := false)
		return string;


	-- Returns the cursor to the segment
	-- where the given point sits on.
	-- Returns the first matching segment only:
	function get_segment (
		contour	: in type_contour;
		point	: in type_vector_model)
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
		vertex	: in type_vector_model)
		return type_neigboring_segments;


	-- Returns the distance from the given reference point to
	-- to the nearest point on the contour.
	-- The point may be inside or outside the contour.
	function get_shortest_distance (
		contour	: in type_contour;
		point	: in type_vector_model)
		return type_float_positive;



	-- Loads the given segments into the given contour.
	-- NOTE: Overwrites already existing segments in the contour.
	procedure load_segments (
		contour		: in out type_contour;
		segments	: in type_segments);
	
	
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
		return type_segments;
	

	-- Returns 1 if the contour consist of just a single circle.
	-- Returns the number of segments if the contour consist of lines
	-- and/or arcs:
	function get_segments_total (
		contour : in type_contour)
		return natural;


	


-- MERGING:

	-- When contours are to be merged, then this
	-- type shall be used to express the result of 
	-- the merge operation:
	type type_merge_result is record
		appended	: boolean := false;
		prepended	: boolean := false;
		successful	: boolean := false;
	end record;
	
	-- Merges two contours to a single one.
	-- 1. Tries to append or prepend source to target
	--    if matching vertices exist.
	-- 2. Modifies the status according to the result
	--    of the operation.
	-- 3. Merges only if both target and source are 
	--    open contours.
	-- 4. If target has no segments (empty) and the source does have
	--    segments, then it just copies the source to the target.
	--    The result has then the flags "successful" and "appended" set true.
	procedure merge_contours (
		target	: in out type_contour;
		source	: in type_contour;
		status	: in out type_merge_result);




	
	

	-- Transposes a contour in Y direction.
	-- Each point of each segment gets shifted by
	-- the formula new_y = offset - old_y:
	procedure transpose_contour (
		contour	: in out type_contour'class;
		offset	: in type_distance);

	

	-- -- Returns the boundaries of the given contour.
	-- function get_boundaries (
	-- 	contour		: in type_contour;
	-- 	line_width	: in type_distance_positive)
	-- 	return type_boundaries;

	-- Computes the bounding-box of a contour
	-- with the given border-wdith:
	function get_bounding_box (
		contour	: in type_contour;
		width	: in type_distance_positive)
		return type_area;


	-- Computes the bounding-box of a contour
	-- with the given border-wdith
	-- taking into account the given offsets,
	-- rotation and mirror style:
	function get_bounding_box (
		contour		: in type_contour;
		width		: in type_distance_positive := 0.0;
		offset_1	: in type_vector_model;
		offset_2	: in type_vector_model := origin;
		rotation	: in type_rotation;
		mirror		: in type_mirror := MIRROR_NO)
		return type_area;



	
	-- A contour must have a properly closed outline.
	-- The outline check returns a list of points.
	-- Each point represents the start of a gap:
	package pac_contour_gaps is new doubly_linked_lists (type_vector_model); 

	
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
	-- If the contour consists of a single circle segment
	-- then the result is true and no gaps are returned.
	-- If the given contour is empty, means if it has no segments at all,
	-- then the result is false and no gaps are returned:
	function is_closed (
		contour	: in type_contour)
		return type_contour_status;


	-- Returns true if the given contour is 
	-- open (means: if it is not closed).
	-- Returns true if the given contour has no segments at all.
	-- Bases on function is_closed (see above):
	function is_open (
		contour	: in type_contour)
		return boolean;
	


	-- Moves a contour by the given offset. 
	procedure move_by ( -- CS rename to move_contour
		contour	: in out type_contour;
		offset	: in type_vector_model);
	

	-- Mirrors a contour along the given axis.
	procedure mirror ( -- CS rename to mirror_contour
		contour	: in out type_contour;
		axis	: in type_mirror := MIRROR_ALONG_Y_AXIS);
	

	-- Rotates a contour about the origin by the given rotation.
	procedure rotate_by ( -- CS rename to rotate_contour
		contour		: in out type_contour;
		rotation	: in type_rotation);



	-- In order to get the status of a point relative to
	-- a contour we need this stuff:
	-- The general approach is:
	-- A ray that starts at point and travels in zero degees 
	-- may intersect the contour edges.
	-- The result of such a query is the type_vector_model_to_contour_status
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
		point	: type_vector_model := origin;
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
		return type_vector_model;

	
	function is_vertex (
		contour	: in type_contour;
		point	: in type_vector_model)
		return boolean;


	
					   
	function get_shortest_distance (
		contour	: in type_contour;
		point	: in type_vector)
		return type_float;


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
		x_position	: type_float;
		segment		: type_intersected_segment;
	end record;


	function "<" (left, right : in type_probe_line_intersection_contour)
		return boolean;


	package pac_probe_line_intersections_contour is new
		doubly_linked_lists (type_probe_line_intersection_contour);


	type type_point_to_contour_status (location : type_location) is record
		-- The point where the probe line has started:
		start			: type_vector_model;

		-- The intersections of the probe line with the polygon edges:
		intersections	: pac_probe_line_intersections_contour.list;

		case location is
			when OUTSIDE | INSIDE =>
				-- The shortest distance of the start point (of the probe line)
				-- to the contour:
				distance : type_float;
				
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
		point		: in type_vector_model)
		return type_point_to_contour_status;


	
	package pac_contour_list is new doubly_linked_lists (type_contour);
	use pac_contour_list; -- CS rename to pac_contours


	-- Iterates the contours. Aborts the process when the proceed-flag goes false:
	procedure iterate (
		contours	: in pac_contour_list.list;
		process		: not null access procedure (position : in pac_contour_list.cursor);
		proceed		: not null access boolean);

	

	-- Moves a list of contours by the given offset:
	procedure move_contours (
		contours	: in out pac_contour_list.list;
		offset		: in type_vector_model);

	
	-- Mirrors a list of contours along the given axis:
	procedure mirror_contours (
		contours	: in out pac_contour_list.list;
		axis		: in type_mirror := MIRROR_ALONG_Y_AXIS);


	-- Rotates a list of contours by the given angle:
	procedure rotate_contours (
		contours	: in out pac_contour_list.list;
		rotation	: in type_rotation);


	-- Returns the first open contour among the
	-- given list of contours:
	function get_first_open (
		contours	: in out pac_contour_list.list)
		return pac_contour_list.cursor;
	




	
end et_geometry_2a.contours;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
