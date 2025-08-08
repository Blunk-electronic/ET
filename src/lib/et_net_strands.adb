------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             NET STRANDS                                  --
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


package body et_net_strands is

	


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



	
	function get_port_count (
		strand	: in type_strand;
		point	: in type_vector_model)
		return natural
	is
		result : natural := 0;

		procedure query_segment (c : in pac_net_segments.cursor) is
			segment : type_net_segment renames element (c);
			count : natural := 0;
		begin
			if get_A (segment) = point then
				count := get_port_count (segment, A);
				
			elsif get_B (segment) = point then
				count := get_port_count (segment, B);
			end if;

			result := result + count;
		end;

		
	begin
		strand.segments.iterate (query_segment'access);
		return result;
	end get_port_count;
		





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





	procedure set_sheet (
		strand	: in out type_strand;
		sheet	: in type_sheet)
	is begin
		set_sheet (strand.position, sheet);
	end;

	



	procedure optimize_strand_1 (
		strand			: in out type_strand;
		log_threshold	: in type_log_level)
	is
		segments_have_been_merged : boolean := true;

		-- The optimization process may require several passes.
		-- In order to avoid a forever-loop this counter is required.
		-- CS: Increase the upper limit if required:
		subtype type_safety_counter is natural range 0 .. 10;
		safety_counter : type_safety_counter := 0;
		

		-- This procedure searches for a net segment (called primary) that
		-- overlaps another segment (called secondary).
		-- Once an overlap has been found:
		-- 1. The search is aborted.
		-- 2. Primary and secondary segment are merged to a new segment.
		-- 3. The secondary segment is removed.
		-- 4. The primary segment is replaced by the new segment.
		procedure search_overlap is

			-- This flag is cleared once an overlap has been found:
			proceed : aliased boolean := true;

			-- After an overlap has been found, these cursors point to the
			-- affected primary and secondary segment:
			primary, secondary : pac_net_segments.cursor;

			-- After merging primary and secondary segment, here the
			-- resulting segment will be stored:
			new_segment : type_net_segment;
			
			
			procedure query_primary (p : in pac_net_segments.cursor) is

				procedure query_secondary (s : in pac_net_segments.cursor) is begin
					-- We do not test the primary segment against itself.
					-- For this reason we compare primary and secondary cursors:
					if s /= p then
						-- Do the actual overlap test. If positive, then
						-- store the cursors of primary and secondary segment
						-- and clear the proceed-flag so that all iterators stop:
						if segments_overlap (s, p) then
							primary := p;
							secondary := s;

							proceed := false;
						end if;
					end if;
				end query_secondary;

			begin
				-- Iterate the secondary segments:
				iterate (strand.segments, query_secondary'access, proceed'access);
			end query_primary;

			
		begin			
			-- Iterate the primary segments. Abort when an overlap has been found:
			iterate (strand.segments, query_primary'access, proceed'access);

			-- If no overlap has been found, then the proceed-flag is still
			-- set. So we notify the caller that nothing has been merged:
			if proceed then
				log (text => "nothing to do", level => log_threshold + 1);
				
				segments_have_been_merged := false;
			else
				log (text => "overlapping segments found", level => log_threshold + 1);
				
				-- If an overlap has been found, then the proceed-flag is cleared.
				-- In this case we merge the two segments:

				-- Merge primary and secondary segment:
				new_segment := merge_overlapping_segments (element (primary), element (secondary));
				
				-- Delete secondary segment, because it is no longer needed:
				strand.segments.delete (secondary);

				-- Replace the primary segment by the new segment:
				strand.segments.replace_element (primary, new_segment);

				-- Notify the caller that a merge took place:
				segments_have_been_merged := true;
			end if;
		end search_overlap;
			
		
	begin
		log (text => "optimize strand", level => log_threshold);
		log_indentation_up;
		
		-- Call procedure search_overlap as many times
		-- as optimzing is required:
		
		while segments_have_been_merged loop
			
			-- Count the loops and raise exception on overflow:
			safety_counter := safety_counter + 1;

			log (text => "pass" & natural'image (safety_counter), level => log_threshold + 1);
			
			log_indentation_up;			
			search_overlap;
			log_indentation_down;
		end loop;

		log_indentation_down;
	end optimize_strand_1;


	

	

	procedure optimize_strand_2 (
		strand			: in out type_strand;
		log_threshold	: in type_log_level)
	is
		segments_have_been_merged : boolean := true;
		
		-- The optimization process may require several passes.
		-- In order to avoid a forever-loop this counter is required.
		-- CS: Increase the upper limit if required:
		subtype type_safety_counter is natural range 0 .. 10;
		safety_counter : type_safety_counter := 0;
		

		-- This procedure searches for a net segment (called primary) that
		-- starts or ends where another segment (called secondary) starts or ends.		
		-- Once a connections has been found:
		-- 1. The search is aborted.
		-- 2. Primary and secondary segment are merged to a new segment.
		-- 3. The secondary segment is removed.
		-- 4. The primary segment is replaced by the new segment.
		procedure search_overlap is

			-- This flag is cleared once an overlap has been found:
			proceed : aliased boolean := true;

			-- After an overlap has been found, these cursors point to the
			-- affected primary and secondary segment:
			primary, secondary : pac_net_segments.cursor;

			-- After merging primary and secondary segment, here the
			-- resulting segment will be stored:
			new_segment : type_net_segment;
			
			
			procedure query_primary (p : in pac_net_segments.cursor) is
				PA : type_vector_model := get_A (p);
				PB : type_vector_model := get_B (p);

				-- The number of segments connected 
				-- with the A end of the primary segment candidate:
				PAS : constant natural := get_length (get_connected_segments (p, A, strand));

				-- The number of segments connected 
				-- with the B end of the primary segment candidate:
				PBS : constant natural := get_length (get_connected_segments (p, B, strand));

				-- Whether the A end of the primary candidate 
				-- segment has any ports connected:
				PAP : constant boolean := has_ports (p, A);

				-- Whether the B end of the primary candidate 
				-- segment has any ports connected:
				PBP : constant boolean := has_ports (p, B);

				
				procedure query_secondary (s : in pac_net_segments.cursor) is 
					SA : type_vector_model := get_A (s);
					SB : type_vector_model := get_B (s);

					-- The number of segments connected 
					-- with the A end of the secondary segment candidate:
					SAS : constant natural := get_length (get_connected_segments (s, A, strand));

					-- The number of segments connected 
					-- with the B end of the secondary segment candidate:
					SBS : constant natural := get_length (get_connected_segments (s, B, strand));

					-- Whether the A end of the secondary segment candidate 
					-- has any ports connected:
					SAP : constant boolean := has_ports (s, A);

					-- Whether the B end of the secondary segment candidate 
					-- has any ports connected:
					SBP : constant boolean := has_ports (s, B);


					procedure do_overlap_test is begin
						log (text => "do overlap test", level => log_threshold + 3);
						-- Do the actual overlap test. If positive, then
						-- store the cursors of primary and secondary segment
						-- and clear the proceed-flag so that all iterators stop.
						-- We test whether the start and end points of the
						-- two segments touch each other:
						if segments_overlap (s, p, test_touch => true) then
							primary := p;
							secondary := s;

							proceed := false;
						end if;
					end do_overlap_test;

					
				begin
					-- We do not test the primary segment against itself.
					-- For this reason we compare primary and secondary cursors:
					if s /= p then
						log (text => "secondary segment: " & to_string (s), level => log_threshold + 1);
						log_indentation_up;

						-- Log segment count and ports on A end:
						log (text => "SAS: " & natural'image (SAS), level => log_threshold + 2);
						log (text => "SAP: " & boolean'image (SAP), level => log_threshold + 2);

						-- Log segment count and ports on B end:
						log (text => "SBS: " & natural'image (SBS), level => log_threshold + 2);
						log (text => "SBP: " & boolean'image (SBP), level => log_threshold + 2);
						
						log_indentation_up;
						
						-- Test primary A against secondary A:
						if PA = SA then
							log (text => "PA = SA", level => log_threshold + 2);
							if PAS = 1 
							and SAS = 1 -- CS no need ?
							and not PAP
							and not SAP
							then
								do_overlap_test;
							end if;

						-- Test primary A against secondary B:
						elsif PA = SB then
							log (text => "PA = SB", level => log_threshold + 2);
							if PAS = 1 
							and SBS = 1 -- CS no need ?
							and not PAP
							and not SBP
							then
								do_overlap_test;
							end if;

						-- Test primary B against secondary A:
						elsif PB = SA then
							log (text => "PB = SA", level => log_threshold + 2);
							if PBS = 1 
							and SAS = 1 -- CS no need ?
							and not PBP
							and not SAP
							then
								do_overlap_test;
							end if;

						-- Test primary B against secondary B:
						elsif PB = SB then
							log (text => "PB = SB", level => log_threshold + 2);
							if PBS = 1 
							and SBS = 1 -- CS no need ?
							and not PBP
							and not SBP
							then
								do_overlap_test;
							end if;
						end if;

						log_indentation_down;
						log_indentation_down;
					end if;
				end query_secondary;

			begin
				log (text => "primary segment: " & to_string (p), level => log_threshold + 1);
				log_indentation_up;

				-- Log segment count and ports on A end:
				log (text => "PAS: " & natural'image (PAS), level => log_threshold + 2);
				log (text => "PAP: " & boolean'image (PAP), level => log_threshold + 2);

				-- Log segment count and ports on B end:
				log (text => "PBS: " & natural'image (PBS), level => log_threshold + 2);
				log (text => "PBP: " & boolean'image (PBP), level => log_threshold + 2);
				
				-- Iterate the secondary segments:
				iterate (strand.segments, query_secondary'access, proceed'access);
				log_indentation_down;
			end query_primary;

			
		begin			
			-- Iterate the primary segments. Abort when an overlap has been found:
			iterate (strand.segments, query_primary'access, proceed'access);

			-- If no overlap has been found, then the proceed-flag is still
			-- set. So we notify the caller that nothing has been merged:
			if proceed then
				log (text => "nothing to do", level => log_threshold + 1);
				
				segments_have_been_merged := false;
			else
				log (text => "overlapping segments found", level => log_threshold + 1);
				
				-- If an overlap has been found, then the proceed-flag is cleared.
				-- In this case we merge the two segments:

				-- Merge primary and secondary segment:
				new_segment := merge_overlapping_segments (element (primary), element (secondary));
				
				-- Delete secondary segment, because it is no longer needed:
				strand.segments.delete (secondary);

				-- Replace the primary segment by the new segment:
				strand.segments.replace_element (primary, new_segment);

				-- Notify the caller that a merge took place:
				segments_have_been_merged := true;
			end if;
		end search_overlap;
			
		

	begin
		log (text => "optimize strand 2", level => log_threshold);
		log_indentation_up;
		
		-- Call procedure search_overlap as many times
		-- as optimzing is required:
		
		while segments_have_been_merged loop
			
			-- Count the loops and raise exception on overflow:
			safety_counter := safety_counter + 1;

			log (text => "pass" & natural'image (safety_counter), level => log_threshold + 1);
			
			log_indentation_up;			
			search_overlap;
			log_indentation_down;
		end loop;

		log_indentation_down;
	end optimize_strand_2;



	
	

	procedure merge_strands (
		target			: in out type_strand;						
		source			: in type_strand;
		joint			: in type_strand_joint;
		log_threshold	: in type_log_level)
	is
		source_copy : type_strand := source;
	begin
		log (text => "merge strands", level => log_threshold);
		log_indentation_up;
		
		case joint.point is
			when TRUE =>
				log (text => "join at point", level => log_threshold + 1);
				null; -- CS 

			when FALSE =>
				log (text => "join via segment", level => log_threshold + 1);
				
				-- Attach the given segment with its
				-- A end to the target strand:
				attach_segment (
					strand			=> target,
					segment			=> joint.segment,
					AB_end			=> A,
					log_threshold	=> log_threshold);

				-- Attach the given segment with its
				-- B end to the source strand:
				attach_segment (
					strand			=> source_copy,
					segment			=> joint.segment,
					AB_end			=> B,
					log_threshold	=> log_threshold);

				-- Splice target and source segments:
				splice (
					target	=> target.segments,
					before	=> pac_net_segments.no_element,
					source	=> source_copy.segments);

				-- Optimize target due to overlapping segments:
				optimize_strand_1 (target, log_threshold);
		end case;
		
		set_strand_position (target);

		log_indentation_down;
	end merge_strands;


	


	function has_ports (
		segment	: in type_connected_segment)
		return boolean
	is begin
		return has_ports (segment.segment, segment.AB_end);
	end;

	

	function get_length (
		segments : in pac_connected_segments.list)
		return natural
	is begin
		return natural (segments.length);
	end;

	

	
	function get_connected_segments (
		primary 	: in pac_net_segments.cursor;
		AB_end		: in type_start_end_point;
		strand		: in type_strand)
		return pac_connected_segments.list
	is
		result : pac_connected_segments.list;

		-- Test a candidate secondary segment whether is
		-- is connected with the given end of the primary segment:
		procedure query_segment (secondary : in pac_net_segments.cursor) is
			sts : type_connect_status;
		begin
			-- Since the primary segment is a member of
			-- the given strand, it must not be tested against itself
			-- and thus must be skipped:
			if secondary /= primary then

				-- Get the connect status of primary and secondary segment:
				sts := get_connect_status (primary, AB_end, secondary);

				-- Depending on the connect status we
				-- collect the secondary segment and its end (A/B)
				-- in the resulting list:
				case sts is
					when CON_STS_A => result.append ((secondary, A));
					when CON_STS_B => result.append ((secondary, B));
					when CON_STS_NONE => null;
				end case;
			end if;
		end;
		
	begin
		-- Iterate through all segments of the given strand:
		strand.segments.iterate (query_segment'access);
		
		return result;
	end get_connected_segments;


	


	
	function get_connected_ports (
		segments	: in pac_connected_segments.list)
		return natural
	is
		result : natural := 0;

		-- Queries a connected segment candidate and adds its
		-- port count to the result:
		procedure query_segment (c : in pac_connected_segments.cursor) is
			use pac_connected_segments;
			segment : type_connected_segment renames element (c);
			ports : natural;
		begin
			ports := get_port_count (element (segment.segment), segment.AB_end); 
			result := result + ports;
		end query_segment;

		
	begin
		-- Iterate through the given connected segments:
		segments.iterate (query_segment'access);

		return result;
	end get_connected_ports;


	


	function has_connected_segments (
		primary 	: in pac_net_segments.cursor;
		AB_end		: in type_start_end_point;
		strand		: in type_strand)
		return boolean
	is
		count : natural;
	begin
		count := get_length (get_connected_segments (primary, AB_end, strand));

		if count = 0 then
			return false;
		else
			return true;
		end if;
	end has_connected_segments;


	



	procedure set_junction (
		strand	: in out type_strand;
		place	: in type_vector_model)
	is
		-- As soon as a segment has been found that starts
		-- or ends at the given place, then this flag is cleared
		-- so that no more segments are searched for:
		proceed : aliased boolean := true;

		-- This will be the segment where the junction
		-- will be set:
		target_segment : pac_net_segments.cursor;

		-- This is the end of the target segment where the 
		-- junction will be set:
		target_AB_end : type_start_end_point;

		-- If the given place is accepted for a junction
		-- then this flag will be set:
		junction_granted : boolean := false;


		-- Queries a given primary segment candidate:
		procedure query_primary_segment (p : in pac_net_segments.cursor) is

			-- This procedure fetches the number of connected
			-- secondary segments and ports (of devices, netchangers and submodules)
			-- and decides whether to grant the junction or not:
			procedure get_count is
				secondary_segments : pac_connected_segments.list;
				s_count : natural; -- the number of connected secondary segments
				p_count_s : natural; -- the number of secondary ports
				p_count_p : natural; -- the number of primary ports
			begin
				-- put_line ("target segment: " & to_string (p));
				proceed := false; -- no more searching requred
				target_segment := p; -- store the affected primary segment

				-- Get the secondary segments which are connected with
				-- the primary segment:
				secondary_segments := get_connected_segments (p, target_AB_end, strand);
				s_count := get_length (secondary_segments);				
				-- put_line ("connected secondary segments " & natural'image (s_count));

				-- Get the number of ports connected with the primary segment:
				p_count_p := get_port_count (element (p), target_AB_end);
				-- put_line ("connected primary ports " & natural'image (p_count_p));

				-- Get the number of ports connected with the secondary segments:
				p_count_s := get_connected_ports (secondary_segments);
				-- put_line ("connected secondary ports " & natural'image (p_count_s));
				
				-- Now the sum of the number of segments and ports
				-- that meet at the given place decides whether to 
				-- place a junction:
				if (1 + s_count + p_count_p + p_count_s) >= 3 then
					junction_granted := true;
				end if;
			end get_count;

			
		begin
			-- Test whether the given place is the
			-- start or end point of the primary candidate segment:
			if get_A (p) = place then
				target_AB_end := A;
				get_count;
				
			elsif get_B (p) = place then
				target_AB_end := B;
				get_count;
			end if;

			-- If no end of the primary candidate segment 
			-- matches the given place, then the next primary
			-- segment will be tested.
		end query_primary_segment;


		
		-- Sets the junction in the target segment
		-- at the given A/B end:
		procedure do_it (segment : in out type_net_segment) is begin
			set_junction (segment, target_AB_end);
		end;
		
		
	begin
		-- put_line ("set junction at " & to_string (place));

		-- Iterate the segments of the given strand. Abort the process
		-- once the proceed-flag is cleared:
		iterate (strand.segments, query_primary_segment'access, proceed'access);


		-- If the junction is allowed at the given place, then
		-- set it in the target segment:
		if junction_granted then
			strand.segments.update_element (target_segment, do_it'access);
		end if;
	end set_junction;


	
	
	

	procedure clear_junctions (
		strand		: in out type_strand;
		segments	: in out pac_connected_segments.list)
	is
		use pac_connected_segments;

		
		procedure query_connected_segment (
			c : in pac_connected_segments.cursor) 
		is
			-- Get the connected segment candidate.
			-- It provides a cursor to the actual segment in the strand
			-- and the related end (A or B):
			connected_segment : type_connected_segment renames element (c);


			-- Clear the junction on the affected end:
			procedure query_segment (
				segment : in out type_net_segment) 
			is begin
				clear_junction (segment, connected_segment.AB_end);
			end query_segment;

			
		begin
			-- Update the segment in the strand:
			strand.segments.update_element (
				position	=> connected_segment.segment,
				process		=> query_segment'access);
		end;


	begin
		-- Iterate though all given connected segments:
		segments.iterate (query_connected_segment'access);
	end clear_junctions;



	
	
	

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




	


	function get_segment_count (
		strand	: in type_strand;
		point	: in type_vector_model)
		return natural
	is
		result : natural := 0;

		
		procedure query_segment (c : in pac_net_segments.cursor) is begin
			-- If either A or B of the candidate segment
			-- is at the given point, then increment the result:
			if get_A (c) = point or get_B (c) = point then
				result := result + 1;
			end if;
		end query_segment;


	begin
		-- Iterate the segments:
		iterate (strand.segments, query_segment'access);
	
		return result;
	end get_segment_count;


	
	


	function has_element (
		segment : in type_segment_to_extend)
		return boolean
	is begin
		return has_element (segment.cursor);
	end;




	function get_segment (
		segment	: in type_segment_to_extend)
		return pac_net_segments.cursor
	is begin
		return segment.cursor;
	end;

		

	function get_end (
		segment	: in type_segment_to_extend)
		return type_start_end_point
	is begin
		return segment.AB_end;
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

				-- Test the A and B end of the candidate segment.
				-- It must be at the attach point
				-- AND it must not have any ports
				-- AND no other segments must exist at the attach point:

				-- A end:
				if get_A (c) = point then
					if not other_segments_exist (
						segments	=> segments,
						except		=> c,
						point		=> point)
					then					
						if not has_ports (c, A) then
							result.cursor := c;
							result.AB_end := A;
							
							proceed := false; -- no more tests required
						end if;
					end if;
					
				-- B end:
				elsif get_B (c) = point then
					if not other_segments_exist (
						segments	=> segments,
						except		=> c,
						point		=> point)
					then
						if not has_ports (c, B) then
							result.cursor := c;
							result.AB_end := B;
					
							proceed := false; -- no more tests required
						end if;
					end if;
				end if;
			end if;
		end query_segment;

		
	begin		
		-- Iterate the given segments. Abort on the
		-- first matching segment. If no segment found,
		-- then the result is no_element:
		iterate (segments, query_segment'access, proceed'access);

		-- If a segment has been found, then return
		-- the result. Otherwise return no_element:
		if has_element (result) then
			return result;
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
			-- to each other. No junction is requred.
			-- The given segment will simply be added
			-- to the other segments of the strand:
			MODE_MAKE_BEND,

			-- The given segment will be attached at the joint
			-- of two already existing segments which run 
			-- run perpedicular to each other.
			-- The given segment will be attached as third segment
			-- to the bend point the two existing segments.
			-- A junction will be set at the affected end of the
			-- given segment:
			MODE_JOIN_BEND_AND_ADD_JUNCTION,

			-- The given segment will be attached at the joint
			-- of three (or more) already existing segments.
			-- The given segment will be attached
			-- to the joint of the existing segments:
			MODE_JOIN_BEND);


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

		

		-- This procedure merges the segment target_to_extend with
		-- the given segment to a single one. Both segments have
		-- the same orientation:
		procedure extend_segment is 

			procedure query_segment (target : in out type_net_segment) is begin
				merge_segments (
					primary			=> target, 
					primary_end		=> get_end (target_to_extend),
					secondary		=> segment,
					secondary_end	=> AB_end);
			end query_segment;
			
		begin
			strand.segments.update_element (
				position	=> get_segment (target_to_extend),
				process		=> query_segment'access);
		end extend_segment;



		-- Attaches the given segment to the strand
		-- and sets on the given segment a junction
		-- at the attach point:
		procedure append_segment_with_junction is
			s : type_net_segment := segment;
		begin
			set_junction (s, AB_end);
			strand.segments.append (s);
		end append_segment_with_junction;


		
		-- Attaches the given segment to the strand:
		procedure append_segment is
		begin
			strand.segments.append (segment);
			-- CS: delete tag labels at attach point
			-- in target strand
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
			-- CASE 1:
			mode := MODE_SPLIT;
			goto label_attach;
		end if;

		
		-- Test case MODE_EXTEND: 
		target_to_extend := get_segment_to_extend (strand.segments, segment, AB_end);

		if has_element (target_to_extend) then
			-- CASE 2:
			mode := MODE_EXTEND;
			goto label_attach;
		end if;


		-- Now, depending on how many segments start or end
		-- at the attach point, we proceed further.		
		
		-- Test case MODE_JOIN_BEND and MODE_JOIN_BEND_WITH_JUNCTION:
		case get_segment_count (strand, point) is
			when 0 => 
				-- This case should never happen:
				raise constraint_error;

				
			when 1 =>
				-- CASE 3:
				-- Only one segment already exists at the attach point.
				-- Then the new segment will simply be added so that both
				-- segments form a bend (the segments are perpedicular to each other).
				-- A junction is not required.
				mode := MODE_MAKE_BEND;

				-- CASE 3.a:
				-- If ports of devices, netchangers or submodules exist
				-- that the attach point, then a junction is required:
				if get_port_count (strand, point) > 0 then
					log (text => "ports at attach point found", level => log_threshold + 1);
					mode := MODE_JOIN_BEND_AND_ADD_JUNCTION;
				end if;
				
				
			when 2 =>
				-- CASE 4:
				-- A bend consisting of two segments (perpedicular to each other)
				-- already exists. 
				-- The new segment will be attached at the bend point.
				-- A junction will be activated on the new segment:
				mode := MODE_JOIN_BEND_AND_ADD_JUNCTION;
				

			when others =>
				-- CASE 5:
				-- More than three segments already exist at the attach point,
				-- It is assumed that a junction is already there.
				-- So a junction is not required.
				mode := MODE_JOIN_BEND;
		end case;
		
		
	<<label_attach>>

		log (text => "mode: " & type_mode'image (mode), level => log_threshold + 1);
		
		case mode is 
			when MODE_SPLIT	=> 
				split_segment;
				
			when MODE_EXTEND =>
				extend_segment;
				
			when MODE_JOIN_BEND_AND_ADD_JUNCTION =>
				append_segment_with_junction;
				
			when others => append_segment;
		end case;

		log_indentation_down;
	end attach_segment;




	

	procedure split_strand (
		strand			: in out type_strand;
		start_segment	: in pac_net_segments.cursor;
		strand_1		: out type_strand;
		strand_2		: out type_strand;
		log_threshold	: in type_log_level)
	is
		use pac_connected_segments;

		-- This procedure has a recursive algorithm.
		-- In order to prevent a forever-loop this 
		-- counter is required:
		subtype type_safety_counter is natural range 0 .. 40; -- CS increase if required
		loops : type_safety_counter := 0;

		
		-- Segments and ports on the ends of the given start segment:
		segments_A, segments_B : pac_connected_segments.list;
		ports_A, ports_B : type_ports;

		start_AB_end : type_start_end_point := A;
		
		-- This is required to indicate which branch of
		-- the given strand is processed:
		subtype type_branch is positive range 1 .. 2;
		branch : type_branch := 1;
		

		-- This is the place where a junction might be required:
		PA, PB : type_vector_model;



		procedure get_segments_and_ports is begin
			-- Get the net segments which are connected with
			-- the A and B end of the given start segment:
			segments_A := get_connected_segments (start_segment, A, strand);
			segments_B := get_connected_segments (start_segment, B, strand);

			-- Get the start and end point of the start segment:
			PA := get_A (start_segment);
			PB := get_B (start_segment);
			
			-- Get the ports which are connected with the
			-- A and B end of the given start segment:
			ports_A := get_ports (start_segment, A);
			ports_B := get_ports (start_segment, B);
		end get_segments_and_ports;

		

		-- Transfers the ports on the A end of the start segment
		-- to one of the connected segments:
		procedure transfer_ports_A is
			con : type_connected_segment;			
			destination_AB : type_start_end_point;
			destination_segment : type_net_segment;
		begin
			-- Get the first segment among those
			-- which are connected with the target segment:
			con := segments_A.first_element;
			destination_segment := element (con.segment);
			destination_AB := con.AB_end;
			append_ports (destination_segment, ports_A, destination_AB);
			strand.segments.replace_element (con.segment, destination_segment);
		end;


		-- Transfers the ports on the B end of the start segment
		-- to one of the connected segments:
		procedure transfer_ports_B is
			con : type_connected_segment;			
			destination_AB : type_start_end_point;
			destination_segment : type_net_segment;
		begin
			-- Get the first segment among those
			-- which are connected with the target segment:
			con := segments_B.first_element;
			destination_segment := element (con.segment);
			destination_AB := con.AB_end;
			append_ports (destination_segment, ports_B, destination_AB);
			strand.segments.replace_element (con.segment, destination_segment);
		end;


		
		
		-- Iterates though the given connected segments and
		-- searches for each of them other connected segments.
		-- This procedure is recursive. It calls itself over and over
		-- until the whole tree of connected segments has been walked through.
		-- A safety counter prevents a stuck-for-ever-loop:
		procedure collect (segments : in pac_connected_segments.list) is

			procedure query_segment (c : in pac_connected_segments.cursor) is
				connected_segment : type_connected_segment renames element (c);
				segment : type_net_segment renames element (connected_segment.segment);
			begin
				log (text => "segment: " & to_string (segment), level => log_threshold + 2);

				-- Depending on the destination strand, append the
				-- candidate segment to the current strand:
				case branch is
					when 1 => strand_1.segments.append (segment);
					when 2 => strand_2.segments.append (segment);
				end case;

				collect (get_connected_segments (
					primary => connected_segment.segment,
					AB_end	=> get_opposide_end (connected_segment.AB_end),
					strand	=> strand));

			end query_segment;
			
		begin
			loops := loops + 1;
			segments.iterate (query_segment'access);
		end collect;



		-- Initiates the search for segments on the A end
		-- of the given start segment:
		procedure walk_along_A_end is begin
			loops := 0; -- reset safety counter

			-- We are on branch 1, means the segments
			-- connected with the A end of the start segment.
			-- Each segment that has been found goes into
			-- list strand_1:
			branch := 1;

			start_AB_end := A;
			log (text => "start at " & to_string (start_AB_end), level => log_threshold + 1);
			log_indentation_up;		

			clear_junctions (strand, segments_A);
			transfer_ports_A;
			collect (segments_A);
			log_indentation_down;
		end;


		-- Initiates the search for segments on the B end
		-- of the given start segment:
		procedure walk_along_B_end is begin
			loops := 0; -- reset safety counter

			-- We are on branch 2, means the segments
			-- connected with the B end of the start segment:
			-- Each segment that has been found goes into
			-- list strand_2:
			branch := 2;
			
			start_AB_end := B;
			log (text => "start at " & to_string (start_AB_end), level => log_threshold + 1);
			log_indentation_up;

			clear_junctions (strand, segments_B);
			transfer_ports_B;
			collect (segments_B);
			log_indentation_down;
		end;

		
		
		-- Deletes the given start segment from the given strand.
		-- CS: Currently not used.
		procedure delete_start_segment is
			c : pac_net_segments.cursor := start_segment;
		begin
			strand.segments.delete (c);
		end;
		

		-- A precheck is required before the actual splitting.
		-- It must be ensured that both ends of the target segment
		-- are connected with other segments. Otherwise there is nothing to do:
		do_split : boolean := true;

		procedure precheck is begin
			if get_length (segments_A) = 0 then
				log (text => "no segments on A end. nothing to do.", level => log_threshold);
				do_split := false;
			end if;

			if get_length (segments_B) = 0 then
				log (text => "no segments on B end. nothing to do.", level => log_threshold);
				do_split := false;
			end if;
		end;

		
		
	begin
		log (text => "split strand. start segment: " & to_string (start_segment), 
			 level => log_threshold);
		
		log_indentation_up;

		get_segments_and_ports;

		precheck;

		-- If the precheck is passed then do the 
		-- actual splitting. Otherwise do nothing:
		if do_split then
			
			-- WALK THOUGH THE SEGMENTS ON THE A END:
			walk_along_A_end;
			
			-- WALK THOUGH THE SEGMENTS ON THE B END:
			walk_along_B_end;

			-- finalize:
			-- CS no need:
			-- delete_start_segment;

			-- Set the junctions (if required) at
			-- the former start and end point of the
			-- start segment:
			set_junction (strand_1, PA);
			set_junction (strand_2, PB);

			-- CS ? optimize_strand_2 (strand, log_threshold + 2);
		end if;

		
		log_indentation_down;
		
		-- CS exception handler for safety counter
	end split_strand;




	


	procedure delete_segment (
		strand			: in out type_strand;
		segment			: in pac_net_segments.cursor;
		empty			: out boolean;
		split			: out boolean;
		strand_1		: out type_strand;
		strand_2		: out type_strand;
		log_threshold	: in type_log_level)
	is
		-- The given segment is now referred to as 
		-- the "target segment" because this will be deleted.
				
		segments_A, segments_B : pac_connected_segments.list;
		segments_A_count, segments_B_count : natural;
		
		ports_A, ports_B : type_ports;

		ports_A_count, ports_B_count : natural;


		procedure get_segments_and_ports is begin
			-- Get the net segments which are connected with
			-- the A and B end of the given segment:
			segments_A := get_connected_segments (segment, A, strand);
			segments_B := get_connected_segments (segment, B, strand);

			segments_A_count := get_length (segments_A);
			segments_B_count := get_length (segments_B);
			
			-- Get the ports which are connected with the
			-- A and B end of the given segment:
			ports_A := get_ports (segment, A);
			ports_B := get_ports (segment, B);

			ports_A_count := get_port_count (ports_A);
			ports_B_count := get_port_count (ports_B);
		end get_segments_and_ports;


		
		-- Splits the given strand. The outcome are two new
		-- strands, stored in the output variables strand_1 and strand_2:
		procedure split_strand is begin
			log (text => "split strand", level => log_threshold + 1);
			log_indentation_up;

			split_strand (
				strand			=> strand,
				start_segment	=> segment,
				strand_1		=> strand_1,
				strand_2		=> strand_2,
				log_threshold	=> log_threshold + 2);

			-- Delete all segments of the given strand:
			strand.segments.clear;
			
			log_indentation_down;
			
			empty := false;
			split := true;
		end split_strand;

		

		-- Deletes the given segment in the given strand:
		procedure trim_strand (AB_end : in type_start_end_point) is
			-- This is a segment that is connected with the target segment.
			-- To this segment the ports of the primary segment will be
			-- transferred. For this reason we refer to this segment
			-- as destination_segment. The affected end of the destination_segment
			-- is the variable destination_AB:
			con : type_connected_segment;			
			destination_AB : type_start_end_point;
			destination_segment : type_net_segment;

			-- This cursor points to the segment to be deleted:
			c : pac_net_segments.cursor := segment;

			-- This is the place where a junction might be required:
			P : type_vector_model;
			
		begin
			log (text => "trim on end " & to_string (AB_end), level => log_threshold + 1);

			case AB_end is
				when A => 
					-- The B end of the target segment is connected with the strand.

					-- put_line ("trim A");
					-- put_line ("segments_B_count" & natural'image (segments_B_count));
					-- put_line ("ports_B_count   " & natural'image (ports_B_count));

					P := get_B (segment);
					
					clear_junctions (strand, segments_B);
					
					-- Get the first segment among those
					-- which are connected with the target segment:
					con := segments_B.first_element;
					destination_segment := element (con.segment);
					destination_AB := con.AB_end;
					
					append_ports (destination_segment, ports_B, destination_AB);

			
				when B => 
					-- The A end of the target segment is connected with the strand.

					-- put_line ("trim B");
					-- put_line ("segments_A_count" & natural'image (segments_A_count));
					-- put_line ("ports_A_count   " & natural'image (ports_A_count));

					P := get_A (segment);
					
					clear_junctions (strand, segments_A);

					-- Get the first segment among those
					-- which are connected with the target segment:
					con := segments_A.first_element;
					destination_segment := element (con.segment);
					destination_AB := con.AB_end;
					
					append_ports (destination_segment, ports_A, destination_AB);
			end case;


			-- Update the destination_segment:
			strand.segments.replace_element (con.segment, destination_segment);
			
			-- Delete the targeted segment:
			strand.segments.delete (c);

			optimize_strand_2 (strand, log_threshold + 2);

			-- Set a junction, as far as requred:
			set_junction (strand, P);
			
			empty := false;
			split := false;
		end trim_strand;
		
		

		-- Deletes the single remaining segment that the given 
		-- strand contains:
		procedure delete_last_segment is begin
			log (text => "delete last segment", level => log_threshold + 1);			
			strand.segments.clear;
			empty := true;
			split := false;
		end;
		
		
	begin
		log (text => "delete segment", level => log_threshold);
		log_indentation_up;

		get_segments_and_ports;

		-- Do some prechecks before the actual deleting process:

		-- CASE 1:
		-- If there are segments on both ends of the target segment,
		-- then the strand will be split in two fragments:
		if segments_A_count > 0 and segments_B_count > 0 then
			split_strand;

		-- CASE 2:
		-- If only the A end has segments connected, then
		-- the strand will be trimmed at the B end of the target segment:
		elsif segments_A_count > 0 and segments_B_count = 0 then
			trim_strand (B);

		-- CASE 3:
		-- If only the B end has segments connected, then
		-- the strand will be trimmed at the A end of the target segment:
		elsif segments_A_count = 0 and segments_B_count > 0 then
			trim_strand (A);

		-- CASE 4:
		-- If no segments are connected with the target segment,
		-- then the segments of the strand will be deleted completely:
		elsif segments_A_count = 0 and segments_B_count = 0 then
			delete_last_segment;
		end if;

		log_indentation_down;
	end delete_segment;

	
	
	

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





	function is_movable (
		strand	: in type_strand;
		segment	: in pac_net_segments.cursor;
		AB_end	: in type_start_end_point)
		return boolean
	is
		result : boolean := true;

		primary_has_ports : boolean;

		-- Segments attached with the given segment
		-- are so called "secondary" segments:
		use pac_connected_segments;
		secondary_segments : pac_connected_segments.list;

		secondary_segments_cursor : pac_connected_segments.cursor;
		
		-- Test a secondary candidate segment whether it has
		-- any ports. If so, then set the result to false.
		-- This aborts the iteration:
		procedure query_segment (c : in type_connected_segment) is begin
			if has_ports (c) then
				result := false;
			end if;
		end query_segment;
		
	begin
		-- At first we test whether the given primary segment
		-- can be moved. If it has ports at the given end, then
		-- it can not be moved. The result is set to false and no
		-- more probing of other segments is required:
		primary_has_ports := has_ports (segment, AB_end);

		if primary_has_ports then
			result := false;
		else
			-- If the primary segment has no ports then we test
			-- the segments connected with the primary segment:
			
			-- Get the secondary segments which are connected with 
			-- the given primary segment at the given AB_end:
			secondary_segments := get_connected_segments (
				primary	=> segment,
				AB_end	=> AB_end,
				strand	=> strand);

			-- Iterate through the secondary segments
			-- and abort on the first segment which is connected
			-- with any port:
			secondary_segments_cursor := secondary_segments.first;

			-- CS use a nice iterator procedure
			
			while has_element (secondary_segments_cursor) loop
				query_element (secondary_segments_cursor, query_segment'access);
				if result = false then
					exit;
				end if;
				next (secondary_segments_cursor);
			end loop;

		end if;
		
		return result;
	end is_movable;


	

	

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


	
end et_net_strands;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
