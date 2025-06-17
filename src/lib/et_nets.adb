------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                                NETS                                      --
--                                                                          --
--                               B o d y                                    --
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
-- <http://www.gnu.org/licenses/>.   
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

with ada.text_io;				use ada.text_io;

with et_conductor_segment.boards;
with et_fill_zones.boards;
with et_vias;
with et_module_names;


package body et_nets is


	function to_string (
		net_count : in type_net_count)
		return string
	is begin
		return type_net_count'image (net_count);
	end to_string;



	function has_segments (
		strand : in type_strand)
		return boolean
	is begin
		if strand.segments.length > 0 then
			return true;
		else
			return false;
		end if;
	end;




	function get_segment_to_split (
		segments	: in pac_net_segments.list;
		point		: in type_vector_model)
		return pac_net_segments.cursor
	is
		result : pac_net_segments.cursor;

		proceed : aliased boolean := true;

		procedure query_segment (c : in pac_net_segments.cursor) is begin
			if between_A_and_B (element (c), point) then
				proceed := false; -- no more test required
				result := c;
			end if;
		end query_segment;
		
	begin
		-- Iterate the given segments. Abort on the
		-- first matching segment. If no segment found,
		-- then the result is no_element:
		iterate (segments, query_segment'access, proceed'access);

		return result;
	end get_segment_to_split;
	





	function other_segments_exist (
		segments	: in pac_net_segments.list;
		except		: in pac_net_segments.cursor;
		point		: in type_vector_model)
		return boolean
	is
		-- This flag goes false as soon as a segment
		-- has been found at the given point:
		proceed : aliased boolean := true;

		
		-- Query a net segment. Skip the segment indicated
		-- by "except":
		procedure query_segment (c : in pac_net_segments.cursor) is begin
			if c /= except then -- do not probe "except"

				-- If either A or B of the candidate segment
				-- is at the given point, then abort the iteration
				-- and return true:
				if get_A (c) = point or get_B (c) = point then
					proceed := false;
				end if;
			end if;
		end query_segment;
		

	begin
		-- Iterate the given segments:
		iterate (segments, query_segment'access, proceed'access);
	
		return not proceed;
	end other_segments_exist;

	



	function has_element (
		segment : in type_segment_to_extend)
		return boolean
	is begin
		return has_element (segment.cursor);
	end;



	
	

	function get_segment_to_extend (
		segments	: in pac_net_segments.list;
		segment		: in type_net_segment;
		AB_end		: in type_start_end_point)
		return type_segment_to_extend
	is
		result : type_segment_to_extend;

		-- In case no suitable segment has been found,
		-- then we returns this:
		result_no_segment : constant type_segment_to_extend := (
			cursor	=> pac_net_segments.no_element,
			AB_end	=> A);													   
		

		proceed : aliased boolean := true;

		-- The point where the given segment will be attached:
		point : type_vector_model := get_end_point (segment, AB_end);
		
		-- The orientation of the given segment:
		orientation : type_line_orientation := get_orientation (segment);

		
		procedure query_segment (c : in pac_net_segments.cursor) is 
			-- Get the orientation of the candidate segment:
			o : type_line_orientation := get_segment_orientation (c);
		begin
			-- We pick out segments of same orientation as
			-- the given segment:
			if o = orientation then

				-- Test the A end of the candidate segment:
				if get_A (c) = point then
					result.cursor := c;
					result.AB_end := A;

					-- CS test ports
					
					proceed := false; -- no more test required

				-- Test the B end of the candidate segment:
				elsif get_B (c) = point then
					result.cursor := c;
					result.AB_end := B;

					-- CS test ports
					
					proceed := false; -- no more test required
				end if;
			end if;
		end query_segment;


		
	begin		
		-- Iterate the given segments. Abort on the
		-- first matching segment. If no segment found,
		-- then the result is no_element:
		iterate (segments, query_segment'access, proceed'access);

		-- If segment has been found, then it must be tested
		-- whether other segments exist a the attach point:
		if has_element (result) then
			
			if other_segments_exist (
				segments	=> segments,
				except		=> result.cursor,
				point		=> point)
			then
				return result_no_segment;
			else
				return result;
			end if;

			-- CS test ports
			
		else
			return result_no_segment;
		end if;
		
	end get_segment_to_extend;

	


	

	procedure attach_segment (
		strand			: in out type_strand;
		segment			: in type_net_segment;
		AB_end			: in type_start_end_point;
		log_threshold	: in type_log_level)
	is 
		-- If a segment is to be split, then this cursor
		-- will be pointing to it:
		target_to_split		: pac_net_segments.cursor;

		-- If a segment is to be extended, then this cursor
		-- will be pointing to it:
		target_to_extend	: type_segment_to_extend;

		
		-- This is the place at which theh given
		-- segment will be joined with the given strand:
		point : type_vector_model;

		-- There are several ways to connect the given segment
		-- with the strand:
		
		type type_mode is (
			-- A single target segment is to be split in two.
			-- Between the two fragments the given segment
			-- will be attached.
			-- The target segment will be modified:							  
			MODE_SPLIT, 

			-- A target segment is to be extended by
			-- the given segment. Both run into the same
			-- direction and will be merged to a single segment.
			-- The target segment will be modified:
			MODE_EXTEND,

			-- The given segment will be attached 
			-- to the target segment. The joint
			-- is a bend point. Both segments run perpedicular
			-- to each other.
			-- The given segment will simply be added
			-- to the other segments of the strand:
			MODE_MAKE_BEND,

			-- The given segment will be attached to
			-- an already existing joint where at least
			-- two existing segments form a bend point.
			-- At least two of the existing segments run
			-- perpedicular to each other.
			-- The given segment will simply be added
			-- to the other segments of the strand:
			MODE_JOIN_BEND,

			-- The given segment will be attached to
			-- an already existing segment that is
			-- connected with a port.
			-- The given segment will simply be added
			-- to the other segments of the strand:
			MODE_JOIN_PORT);


		mode : type_mode := MODE_MAKE_BEND;


		-- Splits the segment indicated by cursor target_to_split
		-- in two segments. 
		-- Deletes the segment indicated by target_to_split because it
		-- will be replaced by the two new segments:
		-- Appends the two fragments to the strand.
		-- Appends the given segment to the strand:
		procedure split_segment is
			fragments : type_split_segment := split_segment (target_to_split, point);
			-- CS: There should be two fragments. Otherwise
			-- exception arises here.

			-- Take a copy of the given segment because
			-- a junction will be activated:
			segment_new : type_net_segment := segment;
		begin
			strand.segments.delete (target_to_split);

			strand.segments.append (fragments.segments (1));
			strand.segments.append (fragments.segments (2));

			-- Activate a junction depending on the end
			-- to be attached:
			case AB_end is
				when A => segment_new.junctions.A := true;
				when B => segment_new.junctions.B := true;
			end case;
			
			strand.segments.append (segment_new);
		end split_segment;

		

		procedure extend_segment is 

			procedure query_segment (target : in out type_net_segment) is
			begin
				null;
			end query_segment;
			
		begin
			strand.segments.update_element (target_to_extend.cursor, query_segment'access);
		end extend_segment;



		procedure append_segment is
		begin
			strand.segments.append (segment);
		end append_segment;
		
		
	begin -- attach_segment
		log (text => "attach segment " & to_string (segment) 
			 & " with " & to_string (AB_end) & " end"
			 & " to strand.", level => log_threshold);

		log_indentation_up;

		
		-- Build the point at which the segment
		-- will be attached:
		point := get_end_point (segment, AB_end);
		log (text => "attach point: " & to_string (point), level => log_threshold + 1);


		
		-- Test case MODE_SPLIT: 
		target_to_split := get_segment_to_split (strand.segments, point);

		if has_element (target_to_split) then
			mode := MODE_SPLIT;
			goto label_attach;
		end if;

		
		-- Test case MODE_EXTEND: 
		target_to_extend := get_segment_to_extend (strand.segments, segment, AB_end);

		if has_element (target_to_extend.cursor) then
			mode := MODE_EXTEND;
			goto label_attach;
		end if;


	<<label_attach>>

		log (text => "mode: " & type_mode'image (mode), level => log_threshold + 1);
		
		case mode is 
			when MODE_SPLIT => split_segment;
			when MODE_EXTEND => extend_segment;
			when others => append_segment;
		end case;

		log_indentation_down;
	end attach_segment;



	
	
	

	procedure set_proposed (
		strand : in out type_strand)
	is begin
		set_proposed (strand.status);
	end;

	
	procedure clear_proposed (
		strand : in out type_strand)
	is begin
		clear_proposed (strand.status);
	end;
		

	function is_proposed (
		strand : in type_strand)
		return boolean
	is begin
		if is_proposed (strand.status) then
			return true;
		else
			return false;
		end if;
	end is_proposed;




	procedure set_selected (
		strand : in out type_strand)
	is begin
		set_selected (strand.status);
	end;

	
	procedure clear_selected (
		strand : in out type_strand)
	is begin
		clear_selected (strand.status);
	end;
		

	function is_selected (
		strand : in type_strand)
		return boolean
	is begin
		if is_selected (strand.status) then
			return true;
		else
			return false;
		end if;
	end is_selected;

	



	procedure modify_status (
		strand		: in out type_strand;
		operation	: in type_status_operation)
	is begin
		modify_status (strand.status, operation);
	end;

	

	procedure reset_status (
		strand		: in out type_strand)
	is begin
		reset_status (strand.status);
	end;


	
	


	function get_sheet (
		strand	: in type_strand)
		return type_sheet
	is begin
		return get_sheet (strand.position);
	end get_sheet;

	


	function get_position (
		strand : in type_strand)
		return type_object_position
	is begin
		return strand.position;
	end;

	

	function get_position (
		strand : in type_strand)
		return string
	is begin
		return to_string (strand.position);
	end;



	function has_segments (
		strand : in pac_strands.cursor)
		return boolean
	is begin
		return has_segments (element (strand));
	end;


	

	function is_proposed (
		strand : in pac_strands.cursor)
		return boolean
	is begin
		return is_proposed (element (strand));
	end;

	
	function is_selected (
		strand : in pac_strands.cursor)
		return boolean
	is begin
		return is_selected (element (strand));
	end;



	

	function get_position (
		strand : in pac_strands.cursor)
		return string
	is begin
		return get_position (element (strand));
	end;


	
	
	procedure iterate (
		strands	: in pac_strands.list;
		process	: not null access procedure (position : in pac_strands.cursor);
		proceed	: not null access boolean)
	is
		c : pac_strands.cursor := strands.first;
	begin
		while c /= pac_strands.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;



	
	
	procedure set_strand_position (
		strand : in out type_strand) 
	is
		point_1, point_2 : type_vector_model;
	
		-- CS: usage of intermediate variables for x/Y of start/end points could improve performance

		procedure query_strand (cursor : in pac_net_segments.cursor) is begin
			-- Test start point of segment. 
			-- if closer to orign than point_1 keep start point
			point_2	:= get_A (cursor);
			if get_distance_absolute (point_2, origin) < get_distance_absolute (point_1, origin) then
				point_1 := point_2;
			end if;

			-- Test start point of segment.
			-- if closer to orign than point_1 keep end point
			point_2	:= get_B (cursor);
			if get_distance_absolute (point_2, origin) < get_distance_absolute (point_1, origin) then
				point_1 := point_2;
			end if;
		end query_strand;

		
	begin
		--log (text => "set strand position");
		
		-- init point_1 as the farest possible point from drawing origin
		point_1 := type_vector_model (set (
					x => type_position_axis'last,
					y => type_position_axis'last));

		-- loop through segments and keep the nearest point to origin
		iterate (strand.segments, query_strand'access);

		-- build and assign the final strand position from point_1
		strand.position.set (point_1);
		
	end set_strand_position;




	

	
	function get_first_segment (
		strand_cursor	: in pac_strands.cursor)
		return pac_net_segments.cursor
	is
		segment_cursor : pac_net_segments.cursor; -- to be returned

		procedure query_segments (strand : in type_strand) is
			segment_position : type_vector_model := far_upper_right;
			
			procedure query_segment (c : in pac_net_segments.cursor) is begin
				if get_A (c) < segment_position then
					segment_position := get_A (c);
					segment_cursor := c;
				end if;

				if get_B (c) < segment_position then
					segment_position := get_B (c);
					segment_cursor := c;
				end if;
			end query_segment;
			
		begin
			iterate (strand.segments, query_segment'access);
		end query_segments;
		
	begin
		query_element (
			position	=> strand_cursor,
			process		=> query_segments'access);
		
		return segment_cursor;
	end get_first_segment;


	

	

	function get_sheet (
		strand_cursor	: in pac_strands.cursor)
		return type_sheet
	is begin
		return get_sheet (element (strand_cursor).position);
	end get_sheet;
		

	
	

	function on_segment (
		segment_cursor	: in pac_net_segments.cursor;
		point			: in type_vector_model)
		return boolean
	is begin
		return element (segment_cursor).on_line (point);
	end on_segment;
	



	

	function on_strand (
		strand_cursor	: in pac_strands.cursor;
		place			: in type_object_position)
		return boolean
	is
		-- This flag goes false if the given point is
		-- on the given strand:
		proceed : aliased boolean := true;

		procedure query_segment (s : in pac_net_segments.cursor) is begin
			--if on_segment (s, type_vector_model (place)) then
			if on_segment (s, place.place) then
				proceed := false; -- abort iteration
			end if;
		end query_segment;
		
	begin
		-- The sheet number of strand and point must match:
		if get_sheet (strand_cursor) = get_sheet (place) then

			-- Probe the segments of the strand:
			iterate (
				segments	=> element (strand_cursor).segments, 
				process		=> query_segment'access,
				proceed		=> proceed'access);

		end if;

		-- Return false if sheet numbers do not match 
		-- or if point is not on any segment of the given strand:
		return (not proceed);
	end on_strand;


	



	procedure set_proposed (
		net : in out type_net)
	is begin
		set_proposed (net.status);
	end;

	
	procedure clear_proposed (
		net : in out type_net)
	is begin
		clear_proposed (net.status);
	end;
		

	function is_proposed (
		net : in type_net)
		return boolean
	is begin
		if is_proposed (net.status) then
			return true;
		else
			return false;
		end if;
	end is_proposed;




	procedure set_selected (
		net : in out type_net)
	is begin
		set_selected (net.status);
	end;

	
	procedure clear_selected (
		net : in out type_net)
	is begin
		clear_selected (net.status);
	end;
		

	function is_selected (
		net : in type_net)
		return boolean
	is begin
		if is_selected (net.status) then
			return true;
		else
			return false;
		end if;
	end is_selected;


	
	

	procedure modify_status (
		net			: in out type_net;
		operation	: in type_status_operation)
	is begin
		modify_status (net.status, operation);
	end;
	




	procedure reset_status (
		net			: in out type_net)
	is begin
		reset_status (net.status);
	end;
	
	


	procedure create_strand (
		net			: in out type_net;
		segment		: in type_net_segment)
	is 
		strand : type_strand; -- the new strand
	begin
		-- Insert the given segment in the new strand:
		strand.segments.append (segment);

		-- Add the new strand to the given net:
		net.strands.append (strand);
	end create_strand;


	

	function has_strands (
		net : in type_net)
		return boolean
	is begin
		if net.strands.length > 0 then
			return true;
		else
			return false;
		end if;
	end;

	
	

	function get_strand (
		net		: in type_net;
		place	: in type_object_position)
		return pac_strands.cursor
	is
		result : pac_strands.cursor := pac_strands.no_element;

		proceed : aliased boolean := true;

		procedure query_strand (s : in pac_strands.cursor) is begin
			if on_strand (s, place) then
				proceed := false;
				result := s;
			end if;
		end query_strand;
		
	begin
		iterate (net.strands, query_strand'access, proceed'access);

		return result;
	end get_strand;


	
	


	function get_strands (
		net		: in type_net;
		sheet	: in type_sheet)
		return pac_strands.list
	is
		result : pac_strands.list;

		procedure query_strand (s : in pac_strands.cursor) is begin
			if get_sheet (s) = sheet then
				result.append (element (s));
			end if;
		end query_strand;
	
	begin
		iterate (net.strands, query_strand'access);

		return result;
	end get_strands;





	

	function get_strands (
		net		: in type_net;
		place	: in type_object_position)
		return pac_strand_cursors.list
	is
		result : pac_strand_cursors.list;

		strand_cursor : pac_strands.cursor := net.strands.first;
		
		
		procedure query_strand (strand : in type_strand) is 
			proceed : aliased boolean := true;
			
			procedure query_segment (segment : in pac_net_segments.cursor) is begin
				if on_segment (get_place (place), segment) then
					result.append (strand_cursor);
					proceed := false; -- no more probing required					
				end if;
			end query_segment;
			

		begin
			-- Look at strands which are on the given sheet:
			if get_sheet (strand) = get_sheet (place) then

				-- Iterate the segments of the strand:
				iterate (strand.segments, query_segment'access, proceed'access);
			end if;
		end query_strand;


	begin
		-- Iterate the strands of the net:
		while has_element (strand_cursor) loop
			query_element (strand_cursor, query_strand'access);
			next (strand_cursor);
		end loop;
		
		return result;
	end get_strands;

	


	

	function get_strands (
		net				: in type_net;
		primary			: in type_net_segment;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return pac_strand_segment_cursors.list
	is
		result : pac_strand_segment_cursors.list;

		
		strand_cursor : pac_strands.cursor := net.strands.first;
		
		
		procedure query_strand (strand : in type_strand) is
			segment_cursor : pac_net_segments.cursor := strand.segments.first;

			-- As soon as a segment has been found, then
			-- this flag is cleared so that no more segments
			-- are tested:
			proceed : boolean := true;
			
			-- Tests whether the candidate segment s ends
			-- either with its A end or its B end on the 
			-- given primary segment:
			procedure query_segment (s : in type_net_segment) is begin
				-- Test the A end of the candidate segment:
				if between_A_and_B (
					primary		=> primary, 
					AB_end		=> A,
					secondary	=> s)
				then
					log (text => "match on " & to_string (A) & " end", level => log_threshold + 1);
					
					-- Append the segment to the result:
					result.append ((strand_cursor, segment_cursor, A));
					proceed := false; -- no more probing required
				end if;

				-- Test the B end of the candidate segment:
				if between_A_and_B (
					primary		=> primary, 
					AB_end		=> B,
					secondary	=> s)
				then
					log (text => "match on " & to_string (B) & " end", level => log_threshold + 1);
					
					-- Append the segment to the result:
					result.append ((strand_cursor, segment_cursor, B));
					proceed := false; -- no more probing required
				end if;
			end query_segment;

				
		begin
			-- Iterate through the segments:
			while has_element (segment_cursor) and proceed loop
				log (text => "segment " & to_string (segment_cursor), level => log_threshold);
				log_indentation_up;
				query_element (segment_cursor, query_segment'access);
				log_indentation_down;
				next (segment_cursor);
			end loop;
		end query_strand;

		
	begin
		-- Iterate through the strands:
		while has_element (strand_cursor) loop
			
			-- We pick out only the strands on the given sheet:
			if get_sheet (strand_cursor) = sheet then
				query_element (strand_cursor, query_strand'access);					
			end if;
			
			next (strand_cursor);
		end loop;

		return result;
	end get_strands;






	function split_segment (
		primary			: in type_net_segment;
		nodes			: in pac_strand_segment_cursors.list;
		log_threshold	: in type_log_level)
		return pac_net_segments.list
	is
		result : pac_net_segments.list;
		
		-- Here we collect all split points:
		split_points : pac_points.list;

		
		-- This procedure computes from a given node
		-- a split point:
		procedure query_node (c : in pac_strand_segment_cursors.cursor) is
			use pac_strand_segment_cursors;
			node : type_strand_segment_cursor renames element (c);
			point : type_vector_model; -- split point
		begin
			case node.AB_end is
				when A => point := get_A (node.segment_cursor);
				when B => point := get_B (node.segment_cursor);
			end case;

			log (text => "split point: " & to_string (point), level => log_threshold + 2);
			
			-- Add the point to the list of split points:
			split_points.append (point);
		end query_node;


		
		procedure make_segments is
			-- Split the given primary segment at
			-- the split points and store the fragments here:			
			f : type_split_line := split_line (primary, split_points);

			-- A candidate segment:
			s : type_net_segment;
		begin
			-- Iterate the fragments and build from each of
			-- them a secondary net segment:
			for i in f.segments'first .. f.segments'last loop

				s := (f.segments (i) with others => <>);
				log (text => to_string (s), level => log_threshold + 2);

				-- CS activate junction ?

				result.append (s);
			end loop;
		end make_segments;

		
		
	begin
		log (text => "split segment: " & to_string (primary), level => log_threshold);
		log_indentation_up;

		-- Iterate the nodes and build a list of split points:
		log (text => "split points: ", level => log_threshold + 1);
		log_indentation_up;
		nodes.iterate (query_node'access);
		log_indentation_down;

		log (text => "create new segments: ", level => log_threshold + 1);
		log_indentation_up;
		make_segments;
		log_indentation_down;
		
		log_indentation_down;
		return result;
	end split_segment;

	

	
	

	procedure delete_strands (
		net		: in out type_net;
		strands	: in pac_strands.list)
	is
		c : pac_strands.cursor := strands.first;

		procedure query_strand (s : in pac_strands.cursor) is
			strand_to_delete : type_strand := element (s);
			cursor : pac_strands.cursor;
		begin
			cursor := find (net.strands, strand_to_delete);
			if cursor /= pac_strands.no_element then
				delete (net.strands, cursor);
			else
				put_line ("WARNING: Delete strands: Strand not found in net !");
				-- CS more details ?
			end if;
		end query_strand;
		
	begin
		iterate (strands, query_strand'access);
	end delete_strands;



	

	procedure merge_strand (
		net		: in out type_net;
		strand	: in type_strand)
	is begin
		append (net.strands, strand);
	end merge_strand;



	

	procedure merge_strands (
		net		: in out type_net;
		strands	: in out pac_strands.list)
	is begin
		splice (
			target	=> net.strands,
			before	=> pac_strands.no_element,
			source	=> strands);
	end merge_strands;



	

	procedure merge_nets (
		net_1	: in out type_net;
		net_2	: in out type_net)
	is
		use et_conductor_segment.boards;
		use pac_conductor_lines;
		use pac_conductor_arcs;

		use et_vias;
		use pac_vias;

		use et_fill_zones.boards;
		use pac_route_solid;
		use pac_route_hatched;
		use pac_cutouts;
		
	begin
		-- SCHEMATIC
		
		-- strands:
		splice (
			target => net_1.strands, 
			before => pac_strands.no_element,
			source => net_2.strands);

		-- BOARD:
		
		-- conductor lines:
		splice (
			target => net_1.route.lines, 
			before => pac_conductor_lines.no_element,
			source => net_2.route.lines);

		-- conductor arcs:
		splice (
			target => net_1.route.arcs, 
			before => pac_conductor_arcs.no_element,
			source => net_2.route.arcs);

		-- vias:
		splice (
			target => net_1.route.vias, 
			before => pac_vias.no_element,
			source => net_2.route.vias);

		-- fill zones:
		splice (
			target => net_1.route.zones.solid, 
			before => pac_route_solid.no_element,
			source => net_2.route.zones.solid);
		
		splice (
			target => net_1.route.zones.hatched, 
			before => pac_route_hatched.no_element,
			source => net_2.route.zones.hatched);

		-- cutout areas:
		-- CS now net specific restrict stuff
		--splice (
			--target => net_1.route.cutouts, 
			--before => pac_cutouts.no_element,
			--source => net_2.route.cutouts);

		
	end merge_nets;


	



	function is_proposed (
		net : in pac_nets.cursor)
		return boolean
	is begin
		return is_proposed (element (net));
	end;


	function is_selected (
		net : in pac_nets.cursor)
		return boolean
	is begin
		return is_selected (element (net));
	end;





	

	function get_net_name (
		net_cursor : in pac_nets.cursor)
		return pac_net_name.bounded_string
	is begin
		return key (net_cursor);
	end;


	

	
	function get_net_name (
		net_cursor : in pac_nets.cursor)
		return string
	is begin
		return to_string (key (net_cursor));
	end get_net_name;


	


	function net_exists (
		net_cursor : in pac_nets.cursor) 
		return boolean 
	is begin
		if net_cursor = pac_nets.no_element then
			return false;
		else 
			return true;
		end if;
	end;


	
	

	procedure iterate (
		nets	: in pac_nets.map;
		process	: not null access procedure (position : in pac_nets.cursor);
		proceed	: not null access boolean)
	is
		c : pac_nets.cursor := nets.first;
	begin
		while c /= pac_nets.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;





	

	function get_ports (
		net		: in pac_nets.cursor;
		variant	: in pac_assembly_variants.cursor := pac_assembly_variants.no_element)
		return type_ports 
	is
		result : type_ports; -- to be returned

		
		procedure query_segments (segment_cursor : in pac_net_segments.cursor) is
			use pac_device_ports;

			use et_netlists;
			use pac_netchanger_ports;			
			use pac_submodule_ports;

			
			-- Inserts the device/port in result.devices. Skips the device/port
			-- according to the given assembly variant.
			procedure query_devices (device_cursor : in pac_device_ports.cursor) is begin
				if et_assembly_variants.is_mounted (
					device		=> element (device_cursor).device_name, -- IC4, R101
					variant		=> variant) 
				then
					--put_line (to_string (element (device_cursor)));
					
					insert (
						container	=> result.devices,
						new_item	=> element (device_cursor));
				end if;

				exception
					when event: others =>
						raise constraint_error with to_string (element (device_cursor))
						--put_line (to_string (element (device_cursor))
						& " already in set !";
						
			end query_devices;

			
		begin
			-- Collect device ports of segment according to given assembly variant.
			iterate (element (segment_cursor).ports.A.devices, query_devices'access);
			iterate (element (segment_cursor).ports.B.devices, query_devices'access);

			-- Ports of netchangers and submodules go into the result right away
			-- because they are not affected by any assembly variants.
			union (result.netchangers, element (segment_cursor).ports.A.netchangers);
			union (result.netchangers, element (segment_cursor).ports.B.netchangers);
			
			union (result.submodules, element (segment_cursor).ports.A.submodules);
			union (result.submodules, element (segment_cursor).ports.B.submodules);
		end query_segments;

		
		
		procedure query_strands (strand_cursor : in pac_strands.cursor) is begin
			iterate (element (strand_cursor).segments, query_segments'access);
		end query_strands;

		
	begin
		--put_line ("net " & to_string (key (net)));		
		iterate (element (net).strands, query_strands'access);
		return result;
	end get_ports;


	



	function has_strands (
		net : in pac_nets.cursor)
		return boolean
	is begin
		return has_strands (element (net));
	end;
	

	

	function get_first_strand (
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor
	is
		strand_cursor : pac_strands.cursor; -- to be returned
		strand_position : type_object_position := greatest_position;

		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is

			procedure query_strand (c : in pac_strands.cursor) is begin
				if element (c).position < strand_position then
					strand_position := element (c).position;
					strand_cursor := c;
				end if;
			end query_strand;
			
		begin			
			iterate (net.strands, query_strand'access);
		end query_strands;

		
	begin -- get_first_strand
		query_element (
			position	=> net_cursor,
			process		=> query_strands'access);
	
		return strand_cursor;
	end get_first_strand;




	

	function get_first_strand_on_sheet (
		sheet		: in type_sheet;
		net_cursor	: in pac_nets.cursor)
		return pac_strands.cursor
	is
		strand_cursor : pac_strands.cursor; -- to be returned

		strand_position : type_object_position := greatest_position;

		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is
			c : pac_strands.cursor := net.strands.first;
		begin			
			while c /= pac_strands.no_element loop

				-- Probe strands on the given sheet only:
				if get_sheet (element (c).position) = sheet then

					if element (c).position < strand_position then
						strand_position := element (c).position;
						strand_cursor := c;
					end if;

				end if;
				
				next (c); -- advance to next strand
			end loop;
		end query_strands;

		
	begin
		query_element (
			position	=> net_cursor,
			process		=> query_strands'access);

		return strand_cursor;
	end get_first_strand_on_sheet;
	





	
	function to_label_rotation (direction : in type_stub_direction) 
		return type_rotation_model is
	begin
		case direction is
			when RIGHT	=> return zero_rotation;
			when LEFT	=> return 180.0;
			when UP		=> return 90.0;
			when DOWN	=> return -90.0;
		end case;
	end to_label_rotation;



	
	
	function stub_direction (
		segment	: in pac_net_segments.cursor;
		point	: in type_vector_model)
		return type_stub 
	is
		is_stub : boolean := true;
		direction : type_stub_direction;
		orientation : constant type_line_orientation := 
			get_segment_orientation (segment);

		-- Get the start and end point of the segment:
		A : constant type_vector_model := get_A (segment);
		B : constant type_vector_model := get_B (segment);
	begin
		case orientation is
			when ORIENT_HORIZONTAL =>
				if get_x (point) >= get_x (A) and
					get_x (point) >= get_x (B) then
					direction := RIGHT;
				end if;

				if get_x (point) <= get_x (A) and
					get_x (point) <= get_x (B) then
					direction := LEFT;
				end if;
				
			when ORIENT_VERTICAL =>
				if get_y (point) >= get_y (A) and
					get_y (point) >= get_y (B) then
					direction := UP;
				end if;

				if get_y (point) <= get_y (A) and
					get_y (point) <= get_y (B) then
					direction := DOWN;
				end if;
				
			when ORIENT_SLOPING =>
				is_stub := false;
		end case;

		if is_stub then
			return (is_stub => TRUE, direction => direction);
		else
			return (is_stub => FALSE);
		end if;

	end stub_direction;


	
end et_nets;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
