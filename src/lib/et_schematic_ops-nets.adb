------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON NETS                          --
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
-- <http://www.gnu.org/licenses/>.                                          --
------------------------------------------------------------------------------

--   For correct displaying set tab with in your edtior to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.strings;						use ada.strings;
with ada.strings.unbounded;				use ada.strings.unbounded;

with et_schematic_ops.units;			use et_schematic_ops.units;
with et_schematic_ops.submodules;

with et_board_ops;						use et_board_ops;
with et_board_ops.ratsnest;				use et_board_ops.ratsnest;


package body et_schematic_ops.nets is

	use pac_text_schematic;
	use pac_net_labels;
	use pac_net_segments;
	use pac_strands;
	use pac_nets;




	
	function get_net_count (
		module		: in pac_generic_modules.cursor)
		return type_net_count
	is
		result : type_net_count := 0;

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			result := type_net_count (length (module.nets));
		end query_nets;
		
	begin
		query_element (module, query_nets'access);
		
		return result;
	end get_net_count;

	



	procedure set_segments_moving (
		module_cursor	: in pac_generic_modules.cursor;
		position		: in type_object_position;							  
		log_threshold	: in type_log_level)
	is

		sheet : constant type_sheet := get_sheet (position);
		place : constant type_vector_model := get_place (position);
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (
					strand : in out type_strand)
				is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (
						segment : in out type_net_segment)
					is begin
						-- Test start point of candidate segment:
						if get_A (segment) = place then
							log (text => "segment (A): " & to_string (segment), level => log_threshold + 2);
							set_A_moving (segment);
						end if;

						-- Test end point of candidate segment:
						if get_B (segment) = place then
							log (text => "segment (B): " & to_string (segment), level => log_threshold + 2);
							set_B_moving (segment);
						end if;
					end query_segment;
					
						
				begin
					-- Iterate the segments of the strand on the given sheet only.
					-- All others strands are skipped:
					if get_sheet (strand) = sheet then
						while has_element (segment_cursor) loop
							strand.segments.update_element (segment_cursor, query_segment'access);
							next (segment_cursor);
						end loop;
					end if;
				end query_strand;

				
			begin
				while has_element (strand_cursor) loop
					net.strands.update_element (strand_cursor, query_strand'access);
					next (strand_cursor);
				end loop;
			end query_net;

			
		begin
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " set start/end points of net segments as moving."
			 & " Reference point " & to_string (position),
			level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end set_segments_moving;





	procedure reset_proposed_segments (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (
					strand : in out type_strand)
				is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (
						segment : in out type_net_segment)
					is begin
						log (text => "segment: " & to_string (segment), level => log_threshold + 2);
						reset_status (segment);
					end query_segment;
					
						
				begin
					-- Iterate the segments of the strand:
					while has_element (segment_cursor) loop
						strand.segments.update_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				while has_element (strand_cursor) loop
					net.strands.update_element (strand_cursor, query_strand'access);
					next (strand_cursor);
				end loop;
			end query_net;

			
		begin
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " reset all net segments.",
			level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_segments;

		

	




	
	function to_string (
		object	: in type_object_segment)
		return string
	is begin
		return "net " & get_net_name (object.net_cursor) 
			-- & " strand " & get_position (object.strand_cursor)
			& " segment " & to_string (object.segment_cursor);
	end;



	function get_sheet (
		object	: in type_object_segment)
		return type_sheet
	is 
		segment : type_net_segment;
	begin
		return get_sheet (object.strand_cursor);
	end;


	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is

				procedure query_strand (strand : in out type_strand) is

					procedure query_segment (seg : in out type_net_segment) is begin
						modify_status (seg, operation);
					end query_segment;
					
				begin
					strand.segments.update_element (segment.segment_cursor, query_segment'access);
				end query_strand;
				
			begin
				net.strands.update_element (segment.strand_cursor, query_strand'access);
			end query_net;
			
		begin
			module.nets.update_element (segment.net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of net segment "
			& get_net_name (segment.net_cursor) 
			& " strand " & get_position (segment.strand_cursor)
			& " " & to_string (segment.segment_cursor)
			& " / " & to_string (operation),
			level => log_threshold);


		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;





	procedure propose_segments (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				strand_cursor : pac_strands.cursor := net.strands.first;

			
				procedure query_strand (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (
						seg : in out type_net_segment)
					is begin
						if in_catch_zone (catch_zone, seg, net_line_width) then
							log (text => "in catch zone", level => log_threshold + 4);
							set_proposed (seg);
							count := count + 1;
						end if;
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) loop
						log (text => to_string (segment_cursor), level => log_threshold + 3);
						log_indentation_up;
						strand.segments.update_element (segment_cursor, query_segment'access);
						log_indentation_down;
						next (segment_cursor);
					end loop;
				end query_strand;
				
				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) loop
					if get_sheet (strand_cursor) = active_sheet then
						log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
						log_indentation_up;
						net.strands.update_element (strand_cursor, query_strand'access);
						log_indentation_down;
					end if;
					next (strand_cursor);
				end loop;				
			end query_net;
			
	
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;
			
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " proposing net segments in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end propose_segments;
	



	


	function get_first_segment (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_segment
	is
		result : type_object_segment;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			proceed : boolean := true;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (strand : in type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;


					procedure query_segment (seg : in type_net_segment) is

						procedure set_result is begin
							result.net_cursor		:= net_cursor;
							result.strand_cursor	:= strand_cursor;
							result.segment_cursor	:= segment_cursor;
							log (text => "match: " & to_string (result), level => log_threshold + 2);
							proceed := false; -- no further probing required
						end set_result;
						
					begin
						case flag is
							when PROPOSED =>
								if is_proposed (seg) then
									set_result;
								end if;
			
							when SELECTED =>
								if is_selected (seg) then
									set_result;
								end if;
			
							when others => null; -- CS
						end case;
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) and proceed loop
						log (text => to_string (segment_cursor), level => log_threshold + 2);
						log_indentation_up;
						query_element (segment_cursor, query_segment'access);
						log_indentation_down;
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) and proceed loop
					log (text => "strand " & get_position (strand_cursor), level => log_threshold + 1);
					log_indentation_up;
					query_element (strand_cursor, query_strand'access);
					log_indentation_down;
					next (strand_cursor);
				end loop;
			end query_net;

			
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) and proceed loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first net segment / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		
		return result;
	end get_first_segment;






	procedure delete_segment (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is


				procedure query_strand (strand : in out type_strand) is
					c : pac_net_segments.cursor := segment.segment_cursor;
				begin
					delete (strand.segments, c);
				end query_strand;

				
			begin
				net.strands.update_element (segment.strand_cursor, query_strand'access);

				-- If the strand has no segments anymore, then
				-- remove the now useless strand entirely:
				if not has_segments (segment.strand_cursor) then
					declare
						c : pac_strands.cursor := segment.strand_cursor;
					begin
						-- CS log message
						delete (net.strands, c);
					end;
				end if;
			end query_net;

			
		begin
			module.nets.update_element (segment.net_cursor, query_net'access);
			
			-- If the net has no strands anymore, 
			-- then delete it entirely because a
			-- net without strands is useless:
			if not has_strands (segment.net_cursor) then
				declare
					c : pac_nets.cursor := segment.net_cursor;
				begin
					-- CS log message
					delete (module.nets, c);
				end;
			end if;			
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " deleting segment " & to_string (segment),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (module_cursor, query_module'access);
		update_strand_positions (module_cursor, log_threshold + 2);
		update_ratsnest (module_cursor, log_threshold + 2);
			
		log_indentation_down;		
	end delete_segment;






	procedure delete_segment (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level) 
	is
		use pac_object_segments;
		segments_in_zone : pac_object_segments.list;
		
		segment : type_object_segment; -- the segment to be deleted
		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " deleting segment in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;

		-- Get all net segments which are in the given zone:
		segments_in_zone := get_segments (module_cursor, sheet, catch_zone, log_threshold + 1);

		-- Issue warning if nothing found in given zone.
		-- Otherwise the first segment that has been found
		-- will be deleted:
		if is_empty (segments_in_zone) then
			log (text => "No segment found at given position !", level => log_threshold + 1);			
		else
			-- From the segments found at the given position, 
			-- take the first one and delete it:
			segment := first_element (segments_in_zone);			
			delete_segment (module_cursor, segment, log_threshold + 2);

			update_strand_positions (module_cursor, log_threshold + 2);

			update_ratsnest (module_cursor, log_threshold + 2);
		end if;
			
		log_indentation_down;		
	end delete_segment;


	


	
	function segment_is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_net_segment;
		point_of_attack	: in type_object_position;
		log_threshold	: in type_log_level) 
		return boolean 
	is
		result : boolean := true;

		-- The zone of the net being attacked:
		zone : type_line_zone;
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " test whether segment " & to_string (segment)
			& " is movable at point of attack" & to_string (get_place (point_of_attack)),
			level => log_threshold);
	
		log_indentation_up;
		
		-- Calculate the zone of attack:
		zone := get_zone (
			point	=> get_place (point_of_attack),
			line	=> segment);

		log (text => "attacked zone " & to_string (zone), level => log_threshold + 1);

		-- Depending on the attached zone we test whether
		-- the A or B end or both ends have any ports:
		case zone is
			when START_POINT =>
				result := not has_ports (segment, A);
				
			when END_POINT =>
				result := not has_ports (segment, B);
				
			when CENTER =>
				result := (not has_ports (segment, A)) and (not has_ports (segment, B));
				-- movable if no ports at A and no ports at B
		end case;

		log_indentation_down;
		
		return result;
	end segment_is_movable;





	

	function segment_is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		AB_end			: in type_start_end_point;
		log_threshold	: in type_log_level) 
		return boolean
	is
		result : boolean := true;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is

				procedure query_strand (strand : in type_strand) is begin
					-- Test the given segment and all connected secondary
					-- segments whether they are movable:
					result := is_movable (strand, segment.segment_cursor, AB_end);
				end query_strand;
				
			begin
				query_element (segment.strand_cursor, query_strand'access);
			end query_net;
			
		begin
			query_element (segment.net_cursor, query_net'access);
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " test whether " & to_string (segment)
			& " is movable at " & to_string (AB_end),
			level => log_threshold);
	
		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
		
		return result;
	end segment_is_movable;


	

	

	function segment_is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_object_segment;
		zone			: in type_line_zone;
		log_threshold	: in type_log_level) 
		return boolean
	is
		result : boolean := false;
	begin
		log (text => "module " & to_string (module_cursor)
			& " test whether " & to_string (segment)
			& " is movable at " & to_string (zone),
			level => log_threshold);

		case zone is
			when START_POINT =>
				if segment_is_movable (module_cursor, segment, A, log_threshold + 1) then
					result := true;
				end if;

			when END_POINT =>
				if segment_is_movable (module_cursor, segment, B, log_threshold + 1) then
					result := true;
				end if;
				
			when CENTER =>
				if  segment_is_movable (module_cursor, segment, A, log_threshold + 1) 
				and segment_is_movable (module_cursor, segment, B, log_threshold + 1) 
				then
					result := true;
				end if;
		end case;

		return result;
	end segment_is_movable;


	

	


	procedure move_secondary_segments (
		module_cursor	: in pac_generic_modules.cursor;
		primary_segment	: in type_object_segment;
		original_segment: in type_net_segment;
		AB_end			: in type_start_end_point;
		displacement	: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is

				-- This procedure searches for segments which are connected
				-- with the given primary segment.
				procedure query_strand (strand : in out type_strand) is
					secondary_segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure move_A_end (
						secondary_segment : in out type_net_segment) 
					is begin
						move_start_by (secondary_segment, displacement);
					end move_A_end;

					
					procedure move_B_end (
						secondary_segment : in out type_net_segment) 
					is begin
						move_end_by (secondary_segment, displacement);
					end move_B_end;

					
					-- The connection status of original primary segment
					-- and candidate secondary segment:
					status : type_connect_status;
				begin
					-- Iterate through the segments of the candidate strand
					-- but skip the given primary segment, because we are 
					-- interested in the segments which are connected with
					-- the primary segment. The cursor to the primary segment
					-- is used in order to identify it among the segments
					-- of the strand. The actual segment state is the one AFTER
					-- the drag operation (see specs of move_secondary_segments),
					-- but the cursor still points to it.
					while has_element (secondary_segment_cursor) loop						
						if secondary_segment_cursor /= primary_segment.segment_cursor then

							-- In order to figure out whether the secondary segment
							-- candidate is connected with the primary segment,
							-- we use the original primary segment because it
							-- provides the state BEFORE the drag operation:
							status := get_connect_status (
								primary		=> original_segment,
								AB_end		=> AB_end, -- the end (A/B) of the primary segment								
								secondary	=> element (secondary_segment_cursor));
							
							-- Depending on the connected end point
							-- of the secondary segment, we now move
							-- the end of the secondary segment.
							-- If the secondary segment is not connected with
							-- the primary segment then nothing happens:
							if status = CON_STS_A then				
								strand.segments.update_element (
									secondary_segment_cursor, move_A_end'access);
							end if;

							if status = CON_STS_B then							
								strand.segments.update_element (
									secondary_segment_cursor, move_B_end'access);
							end if;
							
						end if;
						next (secondary_segment_cursor);
					end loop;
				end query_strand;

				
			begin
				-- We care for the strand of the given primary segment only.
				-- All other strands are irrelevant:
				net.strands.update_element (
					primary_segment.strand_cursor, query_strand'access);
			end query_net;
			
		begin
			module.nets.update_element (
				primary_segment.net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " move secondary net segments connected with primary segment " 
			 & to_string (original_segment),
			level => log_threshold);

		log (text => "at end point " & to_string (AB_end)
			& " by " & to_string (displacement),
			level => log_threshold);

		log_indentation_up;
		
		-- By means of the given primary segment we have prompt access 
		-- to it, because we have a cursor to the net, the strand and to
		-- the actual segment:
		generic_modules.update_element (module_cursor, query_module'access);
		
		log_indentation_down;		
	end move_secondary_segments;


	

	


	function net_segment_at_place (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in type_object_position)
		return boolean 
	is

		-- This flag goes true once a segment has been found.
		segment_found : boolean := false; -- to be returned
		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is

			use pac_nets;			
			net_cursor : pac_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is
				strand_cursor : pac_strands.cursor := net.strands.first;
				
				procedure query_segments (strand : in type_strand) is
					use pac_net_segments;

					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure probe_segment (segment : in type_net_segment) is begin
						-- if place is a start point of a segment
						if get_A (segment) = place.place then
							-- signal iterations in upper level to cancel
							segment_found := true;
						end if;

						-- if place is an end point of a segment
						if get_B (segment) = place.place then
							-- signal iterations in upper level to cancel
							segment_found := true;
						end if;
					end probe_segment;

					
				begin -- query_segments
					while not segment_found and segment_cursor /= pac_net_segments.no_element loop
						query_element (
							position	=> segment_cursor,
							process		=> probe_segment'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;

				
			begin -- query_strands
				while not segment_found and strand_cursor /= pac_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if get_sheet (element (strand_cursor).position) = get_sheet (place) then

						query_element (
							position	=> strand_cursor,
							process		=> query_segments'access);
					
					end if;
					
					next (strand_cursor);
				end loop;
			end query_strands;

			
		begin -- query_nets
			while not segment_found and net_cursor /= pac_nets.no_element loop

				query_element (
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

		
	begin
		-- CS log message
		
		query_element (
			position	=> module_cursor,
			process		=> query_nets'access);
		
		return segment_found;
	end net_segment_at_place;

	

	
	
	

	function get_segments (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_object_segments.list
	is
		use pac_object_segments;
		result : pac_object_segments.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (strand : in type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;
				begin
					-- Iterate through the segments and test
					-- each of them whether it crosses the given zone:
					while has_element (segment_cursor) loop
						log (text => "segment " & to_string (segment_cursor), level => log_threshold + 2);
						
						if on_segment (catch_zone, segment_cursor) then
							log (text => " match", level => log_threshold + 2);
							result.append ((net_cursor, strand_cursor, segment_cursor));
						end if;
						
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				log (text => "net " & to_string (net_name), level => log_threshold + 1);
				log_indentation_up;
				
				-- Iterate through the strands:
				while has_element (strand_cursor) loop
					
					-- We pick out only the strands on the given sheet:
					if get_sheet (strand_cursor) = sheet then
						query_element (strand_cursor, query_strand'access);					
					end if;
					
					next (strand_cursor);
				end loop;

				log_indentation_down;
			end query_net;

			
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " collect net segments on sheet " & to_string (sheet)
			 & "  in zone " & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;

		return result;
	end get_segments;
	





	

	procedure move_primary_segment (
		module_cursor	: in pac_generic_modules.cursor;
		primary_segment	: in type_object_segment;
		POA				: in type_vector_model;
		coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model; -- x/y, the new position 
		zone			: out type_line_zone;
		displacement	: out type_vector_model;
		segment_old		: in out type_net_segment;
		log_threshold	: in type_log_level) 
	is

		-- Get the sheet number of the segment:
		sheet : constant type_sheet := get_sheet (primary_segment);

		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			
			-- Searches the strands of the given net 
			-- for a segment that sits on given point_of_attack.
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is

				
				procedure query_strand (strand : in out type_strand) is

					-- This procedure moves the targeted primary segment
					-- according to the attacked zone:
					procedure move_primary_segment (segment : in out type_net_segment) is

						
						procedure move_primary_segment is 

							procedure move_absolute is begin
								log (text => "move primary segment absolute", level => log_threshold + 2);

								-- The displacement depends on which zone of the
								-- segment is attacked. By the displacement the actual
								-- end point (A/B) will then be moved:
								case zone is
									when START_POINT =>
										displacement := destination - get_A (segment_old);
										move_start_by (segment, displacement);
										
									when END_POINT =>
										displacement := destination - get_B (segment_old);
										move_end_by (segment, displacement);
										
									when CENTER =>
										displacement := destination - POA;
										move_start_by (segment, displacement);
										move_end_by (segment, displacement);
								end case;
							end move_absolute;


							
							procedure move_relative is begin
								log (text => "move primary segment relative", level => log_threshold + 2);
								
								-- Set the displacement required for
								-- secondary segments which will be dragged along.
								-- Since we are dragging relative, the displacement is
								-- the same as the given destination:
								displacement := destination;
								
								case zone is
									when START_POINT =>
										move_start_by (segment, displacement);

									when END_POINT =>
										move_end_by (segment, displacement);
										
									when CENTER =>
										move_start_by (segment, displacement);
										move_end_by (segment, displacement);
								end case;					
							end move_relative;

							
						begin
							-- In order to compute the displacement of secondary
							-- segments, we need a backup of the primary segment
							-- as it is before the move operation:
							segment_old := segment;
							
							log_indentation_up;

							
							
							case coordinates is
								when ABSOLUTE	=> move_absolute;									
								when RELATIVE	=> move_relative;							
							end case;

							-- CS ? move simple net labels along with net segment ?
							-- move_net_labels (
							-- 	segment_before	=> segment_before,
							-- 	segment_after	=> segment,
							-- 	zone			=> zone);
							
							log_indentation_down;
						end move_primary_segment;

						
					begin
						-- Calculate the zone where the segment is being attacked:
						zone := get_zone (segment, POA);
						log (text => "attack segment at " & to_string (zone), level => log_threshold + 1);

						-- If the segment is movable then do the actual move:
						if segment_is_movable (module_cursor, primary_segment, zone, log_threshold + 1) then
							move_primary_segment;
						else
							log (text => "Segment is tied to a port. Dragging not possible !",
								 level => log_threshold + 1);
						end if;
					end move_primary_segment;
					


					-- Looks up ports of devices, netchangers or submodules that are 
					-- to be connected with the segment. The place where ports are
					-- searched depends on the zone that has been moved.
					-- (The given segment sits already at the new position.)
					procedure connect_ports (segment : in out type_net_segment) is
						ports : type_ports;

						-- Append the portlists obtained via function get_ports
						-- to the segment.
						-- CS: Special threatment required if a port is among the portlists
						-- that is already somewhere in the strand. 
						-- This particular port must be exempted from the appending.
						-- Currently only the integrity check (procedure check_integrity)
						-- detects this rare case.

						A_end : constant type_object_position := 
							to_position (get_A (segment), sheet);

						B_end : constant type_object_position := 
							to_position (get_B (segment), sheet);
						
					begin
						case zone is
							when START_POINT =>
								ports := get_ports (
									module_cursor	=> module_cursor, 
									place 			=> A_end,
									log_threshold	=> log_threshold + 1);

								append_ports (segment, ports, A);

								
							when END_POINT =>
								ports := get_ports (
									module_cursor	=> module_cursor, 
									place 			=> B_end,
									log_threshold	=> log_threshold + 1);

								append_ports (segment, ports, B);

								
							when CENTER =>
								ports := get_ports (
									module_cursor	=> module_cursor, 
									place 			=> A_end,
									log_threshold	=> log_threshold + 1);

								append_ports (segment, ports, A);
								
								ports := get_ports (
									module_cursor	=> module_cursor, 
									place 			=> B_end,
									log_threshold	=> log_threshold + 1);
								
								append_ports (segment, ports, B);
						end case;
					end connect_ports;

					
				begin
					strand.segments.update_element (
						primary_segment.segment_cursor, move_primary_segment'access);

					-- If a movement took place then look for ports
					-- which must now be connected with the segment:
					if displacement /= origin then
						-- Look for ports at the start/end points of the segment.
						-- The segment is now at the new position (either start point 
						-- or end point or both).
						-- If any port (of a device, netchanger or submodule) sits there,
						-- then it must be connected with the segment. 
						-- That means adding these ports to the segment.
						strand.segments.update_element (
							primary_segment.segment_cursor, connect_ports'access);
					end if;
				end query_strand;
				
				
			begin
				net.strands.update_element (primary_segment.strand_cursor, query_strand'access);
			end query_net;
			
		begin
			module.nets.update_element (primary_segment.net_cursor, query_net'access);			
		end query_module;


		
		praeamble : constant string := "module " & to_string (module_cursor)
			& " move " & to_string (primary_segment);

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => praeamble & " to" & to_string (destination),
					level => log_threshold);

			when RELATIVE =>
				log (text => praeamble & " by" & to_string (destination),
					level => log_threshold);

		end case;
		
		log_indentation_up;
		generic_modules.update_element (module_cursor, query_module'access);		
		log_indentation_down;
	end move_primary_segment;

	



	
	
		
	procedure drag_segment (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		catch_zone		: in type_catch_zone;
		coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model; -- x/y, the new position 
		log_threshold	: in type_log_level) 
	is
		use pac_object_segments;
		segments_in_zone : pac_object_segments.list;
		primary_segment : type_object_segment; -- the segment being dragged


		-- Other segments which might be connected with the segment
		-- being attacked must be dragged along.
		-- In order to compute the displacement of secondary
		-- segments, we need a backup of the primary segment
		-- as it was before the move operation:
		segment_old : type_net_segment;

		
		-- When the primary segment has been moved, then we get
		-- a certain displacement which is later required to move 
		-- connected secondary segments along:
		displacement : type_vector_model;

		-- The zone at which the segment is being attacked:
		zone : type_line_zone;

		-- The point of attack at which the segment will be grabbed.
		-- It is simply the center of the given catch zone:
		POA : constant type_vector_model := get_center (catch_zone);
		

		praeamble : constant string := "module " & to_string (module_cursor)
			& " dragging segment in " & to_string (catch_zone);

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => praeamble & " to" & to_string (destination),
					level => log_threshold);

			when RELATIVE =>
				log (text => praeamble & " by" & to_string (destination),
					level => log_threshold);

		end case;
		
		log_indentation_up;
		
		-- Get all net segments which are in the given zone:
		segments_in_zone := get_segments (module_cursor, sheet, catch_zone, log_threshold + 1);

		-- Issue warning if nothing found in given zone.
		-- Otherwise the first segment that has been found
		-- will be subjected to a drag operation:
		if is_empty (segments_in_zone) then
			log (text => "No segment found at given position !", level => log_threshold + 1);			
		else
			-- From the segments found at the given position, 
			-- take the first one and subject it to the drag operation:
			primary_segment := first_element (segments_in_zone);		
			
			move_primary_segment (
				module_cursor	=> module_cursor,
				primary_segment	=> primary_segment,
				POA				=> POA,
				coordinates		=> coordinates,
				destination		=> destination,
				zone			=> zone,
				displacement	=> displacement,
				segment_old		=> segment_old,
				log_threshold	=> log_threshold + 2);

			
			-- Move connected secondary segments if the primary
			-- segment has been moved. In this case the displacement is non-zero:
			if displacement /= origin then
				case zone is
					when START_POINT =>
						move_secondary_segments (
							module_cursor	=> module_cursor,
							primary_segment	=> primary_segment,
							original_segment=> segment_old,
							AB_end			=> A,
							displacement	=> displacement,
							log_threshold	=> log_threshold + 1);
							
					when END_POINT =>
						move_secondary_segments (
							module_cursor	=> module_cursor,
							primary_segment	=> primary_segment,
							original_segment=> segment_old,
							AB_end			=> B,
							displacement	=> displacement,
							log_threshold	=> log_threshold + 1);

					when CENTER =>
						move_secondary_segments (
							module_cursor	=> module_cursor,
							primary_segment	=> primary_segment,
							original_segment=> segment_old,
							AB_end			=> A,
							displacement	=> displacement,
							log_threshold	=> log_threshold + 1);

						move_secondary_segments (
							module_cursor	=> module_cursor,
							primary_segment	=> primary_segment,
							original_segment=> segment_old,
							AB_end			=> B,
							displacement	=> displacement,
							log_threshold	=> log_threshold + 1);
				end case;


				-- Update the strand positions:
				update_strand_positions (module_cursor, log_threshold + 1);
				
				-- In case new net-port connections are the 
				-- outcome of the drag operation, then the ratsnest
				-- in the board drawing must be updated:
				update_ratsnest (module_cursor, log_threshold + 1);
			end if;					
		end if;

		
		log_indentation_down;		
	end drag_segment;



	
	


	procedure drag_segment (
		module_cursor	: in pac_generic_modules.cursor;
		primary_segment	: in type_object_segment;
		POA				: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is	
		-- Other segments which might be connected with the segment
		-- being attacked must be dragged along.
		-- In order to compute the displacement of secondary
		-- segments, we need a backup of the primary segment
		-- as it was before the move operation:
		segment_old : type_net_segment;
		
		-- When the primary segment has been moved, then we get
		-- a certain displacement which is later required to move 
		-- connected secondary segments along:
		displacement : type_vector_model;

		-- The zone at which the segment is being attacked:
		zone : type_line_zone;
				
	begin
		log (text => "module " & to_string (module_cursor)
			& " dragging segment " & to_string (primary_segment)
			& " point of attack: " & to_string (POA)
			& " to " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		-- Drag the given primary segment:
		move_primary_segment (
			module_cursor	=> module_cursor,
			primary_segment	=> primary_segment,
			POA				=> POA,
			coordinates		=> ABSOLUTE,
			destination		=> destination,
			zone			=> zone,
			displacement	=> displacement,
			segment_old		=> segment_old,
			log_threshold	=> log_threshold + 2);
		

		
		-- Move connected secondary segments if the primary
		-- segment has been moved. In this case the displacement is non-zero:
		if displacement /= origin then
			case zone is
				when START_POINT =>
					move_secondary_segments (
						module_cursor	=> module_cursor,
						primary_segment	=> primary_segment,
						original_segment=> segment_old,
						AB_end			=> A,
						displacement	=> displacement,
						log_threshold	=> log_threshold + 1);
						
				when END_POINT =>
					move_secondary_segments (
						module_cursor	=> module_cursor,
						primary_segment	=> primary_segment,
						original_segment=> segment_old,
						AB_end			=> B,
						displacement	=> displacement,
						log_threshold	=> log_threshold + 1);

				when CENTER =>
					move_secondary_segments (
						module_cursor	=> module_cursor,
						primary_segment	=> primary_segment,
						original_segment=> segment_old,
						AB_end			=> A,
						displacement	=> displacement,
						log_threshold	=> log_threshold + 1);

					move_secondary_segments (
						module_cursor	=> module_cursor,
						primary_segment	=> primary_segment,
						original_segment=> segment_old,
						AB_end			=> B,
						displacement	=> displacement,
						log_threshold	=> log_threshold + 1);
			end case;


			-- Update the strand positions:
			update_strand_positions (module_cursor, log_threshold + 1);
			
			-- In case new net-port connections are the 
			-- outcome of the drag operation, then the ratsnest
			-- in the board drawing must be updated:
			update_ratsnest (module_cursor, log_threshold + 1);
		end if;					
		

		log_indentation_down;
	end drag_segment;



	

	

	

	function to_string (
		object	: in type_object_strand)
		return string
	is begin
		return "net " & get_net_name (object.net_cursor) 
			& " strand " & get_position (object.strand_cursor);
	end to_string;



	

	
	
	function get_strands (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		place			: in type_object_position;
		log_threshold	: in type_log_level)
		return pac_strand_cursors.list
	is
		use pac_strand_cursors;
		result : pac_strand_cursors.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is begin
				result := get_strands (net, place);
			end query_net;
			
		begin
			query_element (net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " looking up strands of net " & get_net_name (net_cursor)
			 & " at " & to_string (place),
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);

		-- Usually in a correct design strands of the same net
		-- do not cross each other. In such a strange case,
		-- a warning should be output.
		-- Issue a warning if more than one strand has been found:
		if result.length > 1 then
			log (text => " WARNING. More than one strand found at "
				 & to_string (place) & " !", 
				 level => log_threshold);
		end if;
		
		log_indentation_down;

		return result;
	end get_strands;






	function get_strands (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		primary			: in type_net_segment;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)
		return pac_strand_segment_cursors.list
	is
		result : pac_strand_segment_cursors.list;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is begin
				log (text => "net " & to_string (net_name), level => log_threshold + 1);
				log_indentation_up;
				result := get_strands (net, primary, sheet, log_threshold + 2);
				log_indentation_down;
			end query_net;

			
		begin
			query_element (net_cursor, query_net'access);
		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			 & " looking up strands of net " & get_net_name (net_cursor)
			 & " on sheet " & to_string (sheet)
			 & " between A and B of primary segment " & to_string (primary),
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);

		-- Usually in a correct design strands of the same net
		-- do not cross each other. In such a strange case,
		-- a warning should be output.
		-- Issue a warning if more than one strand has been found:
		case result.length is
			when 0 =>
				log (text => "Nothing found.", level => log_threshold + 1);

			when 1 =>
				log (text => "One strand found.", level => log_threshold + 1);

			when others =>
				log (text => "WARNING. More than one strand found at "
					& to_string (primary) & " on sheet " & to_string (sheet) & " !", 
					level => log_threshold + 1);
		end case;
		
		log_indentation_down;

		return result;
	end get_strands;

	
	
	


	function get_strands (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_object_strands.list
	is
		result : pac_object_strands.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is
				strand_cursor : pac_strands.cursor := net.strands.first;
				
				
				procedure query_strand (strand : in type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;
				begin
					-- Iterate through the segments and test
					-- each of them whether it crosses the given zone.
					-- Abort the iteration on the first match:
					while has_element (segment_cursor) loop
						log (text => "segment " & to_string (segment_cursor), level => log_threshold + 2);
						
						if on_segment (catch_zone, segment_cursor) then
							log (text => " match", level => log_threshold + 2);
							result.append ((net_cursor, strand_cursor));
							exit; -- no more probing required
						end if;
						
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				log (text => "net " & to_string (net_name), level => log_threshold + 1);
				log_indentation_up;
				
				-- Iterate through the strands:
				while has_element (strand_cursor) loop
					
					-- We pick out only the strands on the given sheet:
					if get_sheet (strand_cursor) = sheet then
						query_element (strand_cursor, query_strand'access);					
					end if;
					
					next (strand_cursor);
				end loop;

				log_indentation_down;
			end query_net;

			
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " collect net strands on sheet " & to_string (sheet)
			 & "  in zone " & to_string (catch_zone),
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;

		return result;
	end get_strands;


	

	

	procedure reset_strands (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				strand_cursor : pac_strands.cursor := net.strands.first;

				-- As soon as a segment has been found that is in the
				-- catch zone, then the parent strand is set as proposed,
				-- and the probing of segments is cancelled.
				-- This flag is cleared when a segment has been found:
				proceed : boolean := true;
				
				
				procedure query_strand (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (
						seg : in out type_net_segment)
					is begin
						log (text => to_string (seg), level => log_threshold + 4);
						reset_status (strand);
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) loop
						log (text => to_string (segment_cursor), level => log_threshold + 3);
						log_indentation_up;
						strand.segments.update_element (segment_cursor, query_segment'access);
						log_indentation_down;
						next (segment_cursor);
					end loop;					
				end query_strand;
				
				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) loop
					log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
					log_indentation_up;
					net.strands.update_element (strand_cursor, query_strand'access);
					log_indentation_down;
					next (strand_cursor);
				end loop;				
			end query_net;
			
	
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;
			
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " reset net strands",
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end reset_strands;

	

	
	
	

	procedure propose_strands (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				strand_cursor : pac_strands.cursor := net.strands.first;

				-- As soon as a segment has been found that is in the
				-- catch zone, then the parent strand is set as proposed,
				-- and the probing of segments is cancelled.
				-- This flag is cleared when a segment has been found:
				proceed : boolean := true;
				
				
				procedure query_strand (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (
						seg : in out type_net_segment)
					is begin
						if in_catch_zone (catch_zone, seg, net_line_width) then
							log (text => "in catch zone", level => log_threshold + 4);
							set_proposed (strand);
							count := count + 1;
							proceed := false; -- no more probing of segments required
						end if;
					end query_segment;

					
				begin
					-- Iterate through the segments until a segment in the catch
					-- zone has been found:
					while has_element (segment_cursor) and proceed loop
						log (text => to_string (segment_cursor), level => log_threshold + 3);
						log_indentation_up;
						strand.segments.update_element (segment_cursor, query_segment'access);
						log_indentation_down;
						next (segment_cursor);
					end loop;					
				end query_strand;
				
				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) loop
					if get_sheet (strand_cursor) = active_sheet then
						log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
						log_indentation_up;
						net.strands.update_element (strand_cursor, query_strand'access);
						log_indentation_down;
					end if;
					next (strand_cursor);
				end loop;				
			end query_net;
			
	
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;
			
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " proposing net strands in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end propose_strands;
	




	
	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		strand			: in type_object_strand;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is

				procedure query_strand (strand : in out type_strand) is begin
					modify_status (strand, operation);
				end query_strand;
				
			begin
				net.strands.update_element (strand.strand_cursor, query_strand'access);
			end query_net;
			
		begin
			module.nets.update_element (strand.net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of net strand "
			& get_net_name (strand.net_cursor) 
			& " strand " & get_position (strand.strand_cursor)
			& " / " & to_string (operation),
			level => log_threshold);


		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;




	



	function get_first_strand (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_strand
	is 
		result : type_object_strand;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			proceed : boolean := true;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (strand : in type_strand) is

					procedure set_result is begin
						result.net_cursor		:= net_cursor;
						result.strand_cursor	:= strand_cursor;
						log (text => "match: " & to_string (result), level => log_threshold + 2);
						proceed := false; -- no further probing required
					end set_result;
					
				begin
					case flag is
						when PROPOSED =>
							if is_proposed (strand) then
								set_result;
							end if;
		
						when SELECTED =>
							if is_selected (strand) then
								set_result;
							end if;
		
						when others => null; -- CS
					end case;
				end query_strand;

				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) and proceed loop
					log (text => "strand " & get_position (strand_cursor), level => log_threshold + 1);
					log_indentation_up;
					query_element (strand_cursor, query_strand'access);
					log_indentation_down;
					next (strand_cursor);
				end loop;
			end query_net;

			
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) and proceed loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first net strand / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		
		return result;
	end get_first_strand;

	

	

	

	procedure delete_strand (
		module_cursor	: in pac_generic_modules.cursor;
		strand			: in type_object_strand;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				c : pac_strands.cursor := strand.strand_cursor;
			begin
				delete (net.strands, c);
				-- CS log message
			end query_net;

			
		begin
			module.nets.update_element (strand.net_cursor, query_net'access);
			
			-- If the net has no strands anymore, 
			-- then delete it entirely because a
			-- net without strands is useless:
			if not has_strands (strand.net_cursor) then
				declare
					c : pac_nets.cursor := strand.net_cursor;
				begin
					-- CS log message
					delete (module.nets, c);
				end;
			end if;			
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " deleting strand " & to_string (strand),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (module_cursor, query_module'access);
		update_ratsnest (module_cursor, log_threshold + 2);
			
		log_indentation_down;		
	end delete_strand;




	


	procedure delete_strand (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level) 
	is
		use pac_object_strands;
		strands_in_zone : pac_object_strands.list;
		
		strand : type_object_strand; -- the strand to be deleted
		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " deleting strand in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;

		-- Get all strands which are in the given zone:
		strands_in_zone := get_strands (module_cursor, sheet, catch_zone, log_threshold + 1);
  
		-- Issue warning if nothing found in given zone.
		-- Otherwise the first strand that has been found
		-- will be deleted:
		if is_empty (strands_in_zone) then
			log (text => "No strand found at given position !", level => log_threshold + 1);			
		else
			-- From the strands found at the given position, 
			-- take the first one and delete it:
			strand := first_element (strands_in_zone);			
			delete_strand (module_cursor, strand, log_threshold + 2);
  
			update_strand_positions (module_cursor, log_threshold + 2);
  
			update_ratsnest (module_cursor, log_threshold + 2);
		end if;
			
		log_indentation_down;		
	end delete_strand;





	
	


	procedure update_strand_positions (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;
				
				
				procedure query_strand (strand : in out type_strand) is 
					old_position : type_object_position := get_position (strand);
					new_position : type_object_position;
				begin
					log (text => "strand " & get_position (strand), level => log_threshold + 2);
					
					set_strand_position (strand); -- update strand position
					new_position := get_position (strand);

					-- If the position has changed then log
					-- the new position:
					if old_position /= new_position then
						log (text => " new position " & to_string (new_position),
							 level => log_threshold + 2);
					end if;
				end query_strand;
				
				
			begin
				while has_element (strand_cursor) loop
					net.strands.update_element (strand_cursor, query_strand'access);
					next (strand_cursor);
				end loop;
			end query_net;

			
		begin
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " update strand positions",
			level => log_threshold);

		log_indentation_up;
		generic_modules.update_element (module_cursor, query_module'access);
		log_indentation_down;		

	end update_strand_positions;


	

	

	

	function to_string (
		object	: in type_object_net)
		return string
	is begin
		return "net " & get_net_name (object.net_cursor);
	end to_string;



	



	procedure reset_proposed_nets (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				reset_status (net);
			end query_net;

			
		begin
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " reset all nets (each one as a whole).",
			level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_proposed_nets;


	


	
	

	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		net				: in type_object_net;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				modify_status (net, operation);
			end query_net;
			
		begin
			module.nets.update_element (net.net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of whole net "
			& get_net_name (net.net_cursor) 
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;



	

	

	function get_first_net (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_net
	is 
		result : type_object_net;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			proceed : boolean := true;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net) 
			is
				
				procedure set_result is begin
					result.net_cursor := net_cursor;
					log (text => "match: " & to_string (result), level => log_threshold + 1);
					proceed := false; -- no further probing required
				end set_result;
						
			begin
				case flag is
					when PROPOSED =>
						if is_proposed (net) then
							set_result;
						end if;
	
					when SELECTED =>
						if is_selected (net) then
							set_result;
						end if;
	
					when others => null; -- CS
				end case;
			end query_net;

			
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) and proceed loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first net (as a whole) / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		
		return result;
	end get_first_net;
	


	
	

	procedure propose_nets (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				strand_cursor : pac_strands.cursor := net.strands.first;

				-- As soon as a segment has been found, there is no
				-- need to explore more strands or segments of the candidate net.
				-- This flag indicates that the iteration through the strands
				-- is to be aborted:
				proceed : boolean := true;
				
			
				procedure query_strand (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (
						seg : in out type_net_segment)
					is begin
						if in_catch_zone (catch_zone, seg, net_line_width) then
							log (text => "in catch zone", level => log_threshold + 4);
							set_proposed (net);
							proceed := false; -- no more segments need to be probed
							count := count + 1;
						end if;
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) and proceed loop
						log (text => to_string (segment_cursor), level => log_threshold + 3);
						log_indentation_up;
						strand.segments.update_element (segment_cursor, query_segment'access);
						log_indentation_down;
						next (segment_cursor);
					end loop;
				end query_strand;
				
				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) and proceed loop
					if get_sheet (strand_cursor) = active_sheet then
						log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
						log_indentation_up;
						net.strands.update_element (strand_cursor, query_strand'access);
						log_indentation_down;
					end if;
					next (strand_cursor);
				end loop;				
			end query_net;
			
	
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;
			
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " proposing nets in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end propose_nets;



	


	function locate_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string)		
		return pac_nets.cursor 
	is	
		cursor : pac_nets.cursor;

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			cursor := pac_nets.find (module.nets, net_name);
		end query_nets;
		
	begin
		query_element (
			position	=> module_cursor,
			process		=> query_nets'access);
		
		return cursor;
	end locate_net;







	procedure create_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string;
		created			: out boolean;
		net_cursor		: out pac_nets.cursor;
		log_threshold	: in type_log_level)
	is
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			module.nets.insert (
				key			=> net_name,
				position	=> net_cursor,
				inserted	=> created);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " create net " & to_string (net_name),
			level => log_threshold);

		log_indentation_up;
	
		net_cursor := locate_net (module_cursor, net_name);
		
		if has_element (net_cursor) then
			log (text => "net already exists. Nothing to do.",
				 level => log_threshold);
			
			created := false;
			-- Cursor "net_cursor" points to the existing net.
		else
			-- create a new net:
			generic_modules.update_element (module_cursor, query_module'access);

			-- Flag "created" should be set now.
			-- Cursor "net_cursor" should point to the new net now.
		end if;
		
		log_indentation_down;
	end create_net;

	
	
	


	
	function get_lowest_available_anonymous_net (
		module		: in pac_generic_modules.cursor)
		return pac_net_name.bounded_string
	is
		net : pac_net_name.bounded_string; -- like N$56
		cursor : pac_nets.cursor;

		-- This flag goes true once a suitable net
		-- name has been found:
		candiate_found : boolean := false; 
	begin
		-- Propose net names like N$1, N$2, ... and locate them
		-- in the module. The search ends once a net like N$56 can not
		-- be located. This net name would be returned to the caller.
		for i in type_anonymous_net_index'first .. type_anonymous_net_index'last loop

			-- compose net name and locate it in module:
			net := to_anonymous_net_name (i); -- N$1, N$2, ...
			cursor := locate_net (module, net);

			if cursor = pac_nets.no_element then -- not located
				candiate_found := true;
				exit;
			end if;
		end loop;

		if not candiate_found then
			raise constraint_error;
		end if;
		
		return net;
	end get_lowest_available_anonymous_net;
	

	


	

	
	procedure rename_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name_before	: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_name_after	: in pac_net_name.bounded_string; -- RESET_N, MOTOR_ON_OFF_N	
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level) 
	is
		net_cursor_old : pac_nets.cursor; -- points to the old net
		net_cursor_new : pac_nets.cursor; -- points to the new net

		new_net_created : boolean := false;
		
		-- Creates a new empty net named net_name_after. 
		-- Sets the cursor net_cursor_new to the new net.
		-- Sets the flag new_net_created to true.
		procedure create_net (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			insert (
				container	=> module.nets,
				key			=> net_name_after,

				-- The scope of the net assumes the default value LOCAL.
				-- CS: It could be reasonable to assume the scope of the old net.
				new_item	=> (others => <>),
				
				inserted	=> new_net_created,
				position	=> net_cursor_new
				);
		end create_net;

		
		procedure rename_everywhere (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- backup the old net
			net_old	: type_net := element (net_cursor_old);

			procedure copy_net_content (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) -- target
			is begin
				merge_nets (net, net_old);
			end copy_net_content;
			
		begin -- rename_everywhere
			
			-- CS check class and scope !
			
			-- delete the old net entirely:
			delete (
				container	=> module.nets,
				position	=> net_cursor_old);

			-- Merg the old net into the new net:
			update_element (	
				container	=> module.nets,
				position	=> net_cursor_new,
				process		=> copy_net_content'access);
			
		end rename_everywhere;

		
		procedure rename_on_sheet (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- Temporarily collection of affected strands on
			-- the given sheet (of the net to be renamed):
			strands_on_sheet : pac_strands.list;
			
			-- Collects all strands on the targeted sheet in container strands_on_sheet.
			-- Deletes the affected strands from the old net.
			procedure collect_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is begin
				log (text => "collecting strands of net " 
					 & enclose_in_quotes (to_string (net_name)) & " ...",
					 level => log_threshold + 1);
				
				strands_on_sheet := get_strands (net, get_sheet (place));

				log (text => "deleting strands of net " 
					 & enclose_in_quotes (to_string (net_name)) & " ...",
					 level => log_threshold + 1);

				delete_strands (net, strands_on_sheet);
			end collect_strands;

			
			-- Adds the collection of strands strands_on_sheet 
			-- to the targeted net.
			procedure move_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is begin
				add_strands (net, strands_on_sheet);
			end move_strands;

			
		begin -- rename_on_sheet

			-- Collect strands of old net in strands_on_sheet.
			-- Remove strands from old net:
			update_element (
				container	=> module.nets,
				position	=> net_cursor_old,
				process		=> collect_strands'access);

			-- Issue warning if no strands have been collected. This can result:
			-- - from an attempt to rename on a sheet that does not exist 
			-- - from the fact that the targeted sheet does not contain the targeted net 
			if is_empty (strands_on_sheet) then
				log (WARNING, "No strands have been renamed on sheet" & to_string (get_sheet (place)) &
					 ". Check net name and sheet number !");

				if new_net_created then
					-- A net without strands is useless. So the just created net must be discarded.
					log (text => "deleting net " & to_string (net_name_after), level => log_threshold + 1);
					delete (module.nets, net_cursor_new);
				end if;
				
			else
				-- move strands to new net
				update_element (
					container	=> module.nets,
					position	=> net_cursor_new,
					process		=> move_strands'access);
			end if;

			-- If the old net has no strands anymore, delete it.
			if is_empty (element (net_cursor_old).strands) then
				delete (module.nets, net_cursor_old);
			end if;
			
		end rename_on_sheet;

		
		procedure rename_strand (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- The affected strand:
			strand_temp : type_strand;
			
			strand_found : boolean := false;

			-- Locates the strand at place and stores it in strand_temp.
			procedure locate_strand (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor;
			begin
				strand_cursor := get_strand (net, place);
				
				if strand_cursor /= pac_strands.no_element then

					-- fetch strand from old net
					strand_temp := element (strand_cursor);

					-- delete strand in old net
					delete (net.strands, strand_cursor);

					strand_found := true;
				end if;				
			end locate_strand;

			
			-- Moves strand_temp to the targeted net.
			procedure move_strand (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is begin
				add_strand (net, strand_temp);
			end move_strand;

			
		begin -- rename_strand

			-- locate the targeted strand and store it in strand_temp:
			update_element (
				container	=> module.nets,
				position	=> net_cursor_old,
				process		=> locate_strand'access);

			
			if not strand_found then
				log (WARNING, "strand not found at" & to_string (position => place) &
					 ". Check net name and position !");

				if new_net_created then
					-- A net without strands is useless. So the just created net 
					-- must be discarded.
					log (text => "deleting net " & to_string (net_name_after),
						 level => log_threshold + 1);
					
					delete (module.nets, net_cursor_new);
				end if;
				
			else -- strand found
				-- move strand_temp to the targeted net
				update_element (
					container	=> module.nets,
					position	=> net_cursor_new,
					process		=> move_strand'access);
			end if;

			-- If the old net has no strands anymore, delete it.
			if is_empty (element (net_cursor_old).strands) then
				delete (module.nets, net_cursor_old);
			end if;
			
		end rename_strand;

		
	begin -- rename_net
		log (text => "module " & to_string (module_cursor) &
			 " renaming net " & enclose_in_quotes (to_string (net_name_before)) &
			 " to " & enclose_in_quotes (to_string (net_name_after)),
			level => log_threshold);

		-- locate the requested nets in the module
		net_cursor_old := locate_net (module_cursor, net_name_before);
		net_cursor_new := locate_net (module_cursor, net_name_after);		

		-- issue error if old net does not exist:
		if net_cursor_old = pac_nets.no_element then
			net_not_found (net_name_before);
		end if;

		-- if there is no net named net_name_after, notify operator about a new
		-- net being created. 
		if net_cursor_new = pac_nets.no_element then
			log (text => "creating new net " & to_string (net_name_after),
				 level => log_threshold + 1);

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> create_net'access);
		end if;
		-- Now net_cursor_new points to the new net.
		
		log_indentation_up;

		-- log where the renaming will be taking place:
		case scope is
			when EVERYWHERE =>
				log (text => "scope: everywhere -> all strands on all sheets", level => log_threshold);

				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> rename_everywhere'access);

			when SHEET =>
				log (text => "scope: all strands on sheet" & to_string (get_sheet (place)), level => log_threshold);

				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> rename_on_sheet'access);

			when STRAND => 
				log (text => "scope: strand at" & to_string (position => place), level => log_threshold);

				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> rename_strand'access);
				
		end case;

		update_ratsnest (module_cursor, log_threshold + 1);
		
		log_indentation_down;		
	end rename_net;








	
	procedure delete_net (
		module_cursor	: in pac_generic_modules.cursor;
		net				: in type_object_net;
		sheet			: in type_sheet;
		all_sheets		: in boolean := false;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			-- This procedure deletes the whole net on
			-- all sheets:
			procedure delete_whole_net is
				c : pac_nets.cursor := net.net_cursor;
			begin
				module.nets.delete (c);
			end;


			-- This procedure deletes all strands of
			-- the given net on the given sheet only:
			procedure delete_on_sheet is

				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net) 
				is
					strands : pac_strands.list;
				begin
					-- Get the strands on the given sheet:
					strands := get_strands (net, sheet);

					-- Delete the strands on the given sheet:
					delete_strands (net, strands);
				end query_net;

			begin
				module.nets.update_element (net.net_cursor, query_net'access);
				
				-- If the net has no strands anymore, 
				-- then delete it entirely because a
				-- net without strands is useless:
				if not has_strands (net.net_cursor) then
					declare
						c : pac_nets.cursor := net.net_cursor;
					begin
						-- CS log message
						delete (module.nets, c);
					end;
				end if;
			end delete_on_sheet;
			
			
		begin
			case all_sheets is
				when TRUE	=> delete_whole_net;
				when FALSE	=> delete_on_sheet;
			end case;			
		end query_module;

		
	begin
		if all_sheets then
			log (text => "module " & to_string (module_cursor)
				& " deleting net " & to_string (net)
				& " on all sheets.",
				level => log_threshold);

		else
			log (text => "module " & to_string (module_cursor)
				& " deleting net " & to_string (net)
				& " on sheet " & to_string (sheet) & ".",
				level => log_threshold);

		end if;


		log_indentation_up;
		
		generic_modules.update_element (module_cursor, query_module'access);
		update_ratsnest (module_cursor, log_threshold + 1);
			
		log_indentation_down;		
	end delete_net;






	
	procedure delete_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level) 
	is
		net_cursor : pac_nets.cursor; -- points to the net

		
		procedure delete_everywhere (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			delete (
				container	=> module.nets,
				position	=> net_cursor);
		end;

		
		procedure delete_on_sheet (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			
			procedure delete_strands_of_sheet (
			-- Removes the affected strands from the net.
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;
				strand_count_before : count_type := length (net.strands);
			begin
				-- Look at the strands that are on the targeted sheet.
				while strand_cursor /= pac_strands.no_element loop
					if get_sheet (element (strand_cursor).position) = get_sheet (place) then
						delete (net.strands, strand_cursor);
					end if;
					next (strand_cursor);
				end loop;

				-- Issue warning if no strands have been deleted. This can result:
				-- - from an attempt to rename on a sheet that does not exist 
				-- - from the fact that the targeted sheet does not contain the targeted net 
				-- This simple check is a compare of the number of strands before with the
				-- number of strands after the deletion:
				if length (net.strands) = strand_count_before then -- nothing deleted
					log (WARNING, "no strands have been deleted on sheet" & to_string (get_sheet (place)) &
						". Check net name and sheet number !");
				end if;
			end;

			
		begin -- delete_on_sheet

			-- delete strands in net
			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> delete_strands_of_sheet'access);

			-- If the net has no strands anymore, delete it.
			if is_empty (element (net_cursor).strands) then
				delete (module.nets, net_cursor);
			end if;
			
		end delete_on_sheet;
		

		
		procedure delete_strand (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			strand_found : boolean := false;

			
			procedure locate_strand (
			-- Locates the strand that starts at place.
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;
			begin
				-- Find the strand that starts at the given position.
				while strand_cursor /= pac_strands.no_element loop
					if element (strand_cursor).position = place then
						-- CS: if place is not exactly the start position of the strand,
						-- search for any other point on the strand instead.

						-- delete strand in net
						delete (net.strands, strand_cursor);

						strand_found := true;
						-- no need for further searching
						exit;
					end if;
					next (strand_cursor);
				end loop;
			end locate_strand;

			
		begin -- delete_strand

			-- locate the targeted strand
			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> locate_strand'access);

			if not strand_found then
				log (WARNING, "strand not found at" & to_string (position => place) &
					 ". Check net name and position !");
			end if;

			-- If the net has no strands anymore, delete it.
			if is_empty (element (net_cursor).strands) then
				delete (module.nets, net_cursor);
			end if;
			
		end delete_strand;

		
	begin -- delete_net		
		log (text => "module " & to_string (module_cursor)
			& " deleting net " & to_string (net_name),
			level => log_threshold);

		-- locate the requested nets in the module
		net_cursor := locate_net (module_cursor, net_name);

		-- issue error if net does not exist:
		if net_cursor = pac_nets.no_element then
			net_not_found (net_name);
		end if;

		log_indentation_up;

		-- show where the deletion will be taking place:
		case scope is
			when EVERYWHERE =>
				log (text => "scope: everywhere -> all strands on all sheets", level => log_threshold);

				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> delete_everywhere'access);

			when SHEET =>
				log (text => "scope: all strands on sheet" & to_string (get_sheet (place)), level => log_threshold);

				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> delete_on_sheet'access);

			when STRAND => 
				log (text => "scope: strand at" & to_string (position => place), level => log_threshold);

				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> delete_strand'access);
				
		end case;

		update_ratsnest (module_cursor, log_threshold + 1);
		
		log_indentation_down;		
	end delete_net;



	


	procedure delete_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string;
		sheet			: in type_sheet;
		all_sheets		: in boolean := false;
		log_threshold	: in type_log_level)
	is
		net : type_object_net;

	begin
		if all_sheets then
			log (text => "module " & to_string (module_cursor)
				& " deleting net " & to_string (net_name)
				& " on all sheets.",
				level => log_threshold);

		else
			log (text => "module " & to_string (module_cursor)
				& " deleting net " & to_string (net_name)
				& " on sheet " & to_string (sheet) & ".",
				level => log_threshold);

		end if;

		log_indentation_up;

		-- Locate the requested net in the module
		net.net_cursor := locate_net (module_cursor, net_name);

		-- If the requested net exists, then delete it. 
		-- Otherwise nothing happens:
		if has_element (net.net_cursor) then
			delete_net (module_cursor, net, sheet, all_sheets, log_threshold + 1);			
		else
			log (text => "Net does not exist !", level => log_threshold);
		end if;		
			
		log_indentation_down;
	end delete_net;



	
	
	

	procedure show_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				set_selected (net);
			end query_net;
			
		begin
			module.nets.update_element (net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " showing/highlight whole net "
			& get_net_name (net_cursor),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end show_net;

	
	



	

	
	

	function get_first_net (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_net_name.bounded_string
	is
		result : pac_net_name.bounded_string;
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			result := key (module.nets.first);
		end query_module;

	begin
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		return result;

		exception
			when others => 
				raise semantic_error_1 with
					"ERROR: No net found in module !";
			
	end get_first_net;




	
	
	function get_nets (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
		return pac_net_names.list
	is
		result : pac_net_names.list;

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			procedure query_net (c : in pac_nets.cursor) is begin
				result.append (key (c));
			end query_net;
			
		begin
			iterate (module.nets, query_net'access);
		end query_module;


	begin
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) &
			 " collecting all nets",
			 level => log_threshold);

		log_indentation_up;

		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		return result;
	end get_nets;




	
	function get_net_index (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string;
		log_threshold	: in type_log_level)		
		return type_net_index
	is
		index : type_net_index := 0;

		use pac_net_names;
		nets : pac_net_names.list;
		c : pac_net_names.cursor;
	begin
		-- Fetch the names of all nets (alphabetically sorted):
		nets := get_nets (module_cursor, log_threshold + 1);

		-- Iterate the nets until the given net has been found:
		c := nets.first;
		while c /= pac_net_names.no_element loop
			if net_name = element (c) then
				exit;
			end if;
			
			index := index + 1;
			
			next (c);
		end loop;

		-- If the given net has not been found, raise exception:
		if c = pac_net_names.no_element then
			raise constraint_error;
		end if;
		
		return index;
	end get_net_index;




	
	
	function get_nets_at_place (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in type_object_position;
		log_threshold	: in type_log_level)
		return pac_net_names.list 
	is
		use pac_net_names;
		result : pac_net_names.list; -- to be returned

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is

			procedure query_nets (net_cursor : in pac_nets.cursor) is
				net : type_net := element (net_cursor);

				-- Cnce a segment has been found at the given
				-- place, then this flag is cleared so that
				-- probing of segments of a strand is cancelled:
				proceed : boolean := true;
				
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_segments (strand : in type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;
				begin
					while has_element (segment_cursor) loop
						log (text => to_string (segment_cursor), level => log_threshold + 2);

						if on_line (
							point 	=> place.place,
							line	=> element (segment_cursor)) then
						
							log (text => "match", level => log_threshold + 2);

							proceed := false; -- signals the calling unit to cancel the search

							-- store net name in return value
							append (result, key (net_cursor));

							exit; -- no need to search for more segments in this strand
						end if;
						
						next (segment_cursor);
					end loop;
				end query_segments;

				
			begin
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				
				-- Iterate through in strands of the candidate net. 
				-- Cancel the search after the first matching segment.
				while has_element (strand_cursor) and proceed loop

					-- Look at strands on the given sheet only:
					if get_sheet (strand_cursor) = get_sheet (place) then
						log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
						
						log_indentation_up;						
						query_element (strand_cursor, query_segments'access);
						log_indentation_down;
					end if;
					
					next (strand_cursor);
				end loop;
				
				log_indentation_down;
			end query_nets;				

			
		begin
			iterate (module.nets, query_nets'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) &
			 " identifying nets at" & to_string (position => place),
			 level => log_threshold);

		log_indentation_up;

		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		return result;
	end get_nets_at_place;







	
	function segments_touch_foreign_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		sheet			: in type_sheet;
		segments		: in pac_net_segments.list;
		log_threshold	: in type_log_level)
		return boolean
	is
		-- If none of the given segments touches a foreign
		-- net, then this flag remains set until the end of this
		-- function. So this flag will be negated on return:
		proceed : aliased boolean := true;


		-- This procedure takes a candidate segment (that is to be inserted)
		-- and tests its A and B end whether it meets a foreign net:
		procedure test_segment (c : in pac_net_segments.cursor) is

			-- This is the actual place where the test is performed:
			place : type_object_position;


			-- This procedure iterates the nets of the module,
			-- but skips the given target net:
			procedure query_module (
				module_name	: in pac_module_name.bounded_string;
				module		: in type_generic_module) 
			is

				procedure query_net (n : in pac_nets.cursor) is
					net : type_net renames element (n);
				begin
					if n /= net_cursor then -- skip the given target net
						log (text => "foreign net " & get_net_name (n), level => log_threshold + 2);

						-- Test whether the candidate net has a segment that
						-- starts or ends at the given place.
						-- If that is the case, then no more tests are
						-- required. The flag "proceed" is cleared
						-- so that all iterators are stopped:
						if has_end_point (net, place) then
							log (text => " segment found at " & to_string (place),
								level => log_threshold + 2);
							proceed := false;
						end if;
					end if;
				end query_net;

			begin
				log_indentation_up;
				iterate (module.nets, query_net'access, proceed'access);
				log_indentation_down;
			end query_module;


		begin
			log (text => "probing segment: " & to_string (c), level => log_threshold + 1);
			log_indentation_up;

			-- Test the A end of the candidate segment:
			log (text => "A end: " & to_string (get_A (c)), level => log_threshold + 2);
			set_sheet (place, sheet);
			set_place (place, get_A (c));

			-- Iterate the nets of the module. Skip the given net.
			query_element (module_cursor, query_module'access);

			-- If no touching segment found, then test the B end 
			-- of the candidate segment:
			if proceed then
				log (text => "B end: " & to_string (get_B (c)), level => log_threshold + 2);
				set_sheet (place, sheet);
				set_place (place, get_B (c));

				-- Iterate the nets of the module. Skip the given net.
				query_element (module_cursor, query_module'access);
			end if;

			log_indentation_down;
		end test_segment;
			

	begin
		log (text => "module " & to_string (module_cursor) 
			& " test segments of net " & get_net_name (net_cursor)
			& " on sheet " & to_string (sheet)
			& " against foreign nets.",
			 level => log_threshold);

		log_indentation_up;

		-- Iterate the given segments. For each segment, its start
		-- and end point is tested whether it meeets a segment of a
		-- foreign net:
		iterate (segments, test_segment'access, proceed'access);

		log_indentation_down;

		return not proceed;
	end segments_touch_foreign_net;






	procedure insert_net_segments (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		sheet			: in type_sheet;
		segments		: in pac_net_segments.list;
		log_threshold	: in type_log_level)
	is

		procedure query_segment (c : in pac_net_segments.cursor) is
			segment : type_net_segment renames element (c);

			-- The segment to be inserted can touch
			-- with its ends other already existing strands belonging
			-- to the targeted net. So we need two lists:
			-- One for the strands found at the A end of the segment,
			-- and another for the strands found at the B end:
			use pac_strand_cursors;
			strands_at_A, strands_at_B : pac_strand_cursors.list;
			

			-- This procedure searches for strands (of the given
			-- net) that run across the A or B end of the
			-- given segment:
			procedure get_strands_at_AB is
				place : type_object_position;
			begin
				place := to_position (get_A (segment), sheet);			
				strands_at_A := get_strands (module_cursor, net_cursor, place, log_threshold + 2);

				place := to_position (get_B (segment), sheet);
				strands_at_B := get_strands (module_cursor, net_cursor, place, log_threshold + 2);
			end get_strands_at_AB;



			procedure query_module (
				module_name	: in pac_module_name.bounded_string;
				module		: in out type_generic_module) 
			is

				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net)
				is
					type type_insert_mode is (
						-- The given segment has no connection with any strand.
						-- For the given segment a new strand will be created:
						NEW_STRAND,	

						-- The given segment will be connected at its A end
						-- with an existing strand:
						ATTACH_A,
					
						-- The given segment will be connected at its B end
						-- with an existing strand:
						ATTACH_B,

						ATTACH_A_AND_B);
											

					-- The insert_mode will be determined after
					-- evaluation of the lists strands_at_A/B and
					-- strands_between_AB:
					insert_mode : type_insert_mode;

					-- In case the segment is to be connected with a strand,
					-- then this cursor will point to the target strand:
					strand_cursor : pac_strands.cursor;


					-- This procedure connects the open ends (both A and B)
					-- of the segment with ports of devices, netchangers and
					-- submodules that might exist there. Since the segment
					-- is the seed for a new strand, both ends of the segment
					-- are assumed to be open (means not conneted yet with anything):
					procedure create_new_strand is
						-- Take a copy of the candidate segment:
						s : type_net_segment := segment;

						-- This is the place where ports are searched for:
						place : type_object_position;

						-- Here the ports will be stored Temporarily:
						ports : type_ports;
					begin
						log_indentation_up;

						-- Set the sheet for the place to search for ports:
						set_sheet (place, sheet);

						-- Search for ports on both ends of the segment:

						-- Get the ports of devices, netchangers and submodules
						-- at the B end. Assign the ports to the B end:
						set_place (place, get_B (segment));
						ports := get_ports (module_cursor, place, log_threshold + 4);
						s.ports.B := ports;

						-- Get the ports of devices, netchangers and submodules
						-- at the A end. Assign the ports to the A end:
						set_place (place, get_A (segment));
						ports := get_ports (module_cursor, place, log_threshold + 4);
						s.ports.A := ports;

						-- Create the new strand with the new segment:
						create_strand (net, s);

						log_indentation_down;
					end create_new_strand;


					-- This procedure attaches the candidate segment with the
					-- A or B end to the given strand. 
					-- It also connects the open end (opposide of the end to be connected)
					-- of the segment with ports of devices, netchangers and
					-- submodules that might exist there.
					-- The opposide end is always assumed as open (means there is nothing
					-- connected with it yet):
					procedure query_strand (strand : in out type_strand) is
						-- Take a copy of the candidate segment:
						s : type_net_segment := segment;

						-- This is the place where ports are searched for:
						place : type_object_position;

						-- Here the ports will be stored temporarily:
						ports : type_ports;
					begin
						log_indentation_up;

						-- Set the sheet for the place to search for ports:
						set_sheet (place, sheet);

						if insert_mode = ATTACH_A then
							-- Get the ports of devices, netchangers and submodules
							-- at the B end. Assign the ports to the B end:
							set_place (place, get_B (segment));
							ports := get_ports (module_cursor, place, log_threshold + 4);
							s.ports.B := ports;

							-- Attach the segment to the strand:
							attach_segment (strand, s, A, log_threshold + 4);
						end if;

							
						if insert_mode = ATTACH_B then
							-- Get the ports of devices, netchangers and submodules
							-- at the A end. Assign the ports to the A end:
							set_place (place, get_A (segment));
							ports := get_ports (module_cursor, place, log_threshold + 4);
							s.ports.A := ports;

							-- Attach the segment to the strand:
							attach_segment (strand, s, B, log_threshold + 4);
						end if;

						log_indentation_down;
					end query_strand;



					procedure attach_segment_at_both_ends is
						strand_at_A, strand_at_B : pac_strands.cursor;
					begin
						log (text => "Attach A and B end of segment to strand.", level => log_threshold + 3);

						-- Take the first strand of the strands at the
						-- A end of the new segment:
						strand_at_A := first_element (strands_at_A);

						-- Take the first strand of the strands at the
						-- B end of the new segment:
						strand_at_B := first_element (strands_at_B);

						if strand_at_A = strand_at_B then
							-- Both strands are equal then CASE 4.a applies:
							-- The new segment will be rejected.
							log (text => "Segment rejected (Would cause a loop.) !", level => log_threshold + 3);
							-- CS Output a warning.
						else
							-- Different strands. CASE 4.b applies:
							log (text => "Merge strand on B end with strand on A end.", level => log_threshold + 3);
							log_indentation_up;

							-- Merge the strand at the B end with the strand at the A end.
							-- The given segment serves a joint between the two strands.
							-- The strand at the B end will be removed:
							merge_strands (
								net				=> net,
								target			=> strand_at_A,
								source			=> strand_at_B,
								joint			=> ((point => false, segment => segment)),
								log_threshold	=> log_threshold + 4);

							log_indentation_down;
						end if;
					end attach_segment_at_both_ends;


				begin
					-- CASE 1:
					-- The new segment has no connection with any other strand.
					-- If no strands exist on any end of the 
					-- new segment, then a new strand must be created.
					-- The new strand will then contain the given segment:
					if is_empty (strands_at_A) and is_empty (strands_at_B) then
						insert_mode := NEW_STRAND;
					end if;

					-- CASE 2:
					-- The new segment starts (A) at an existing strand.
					-- The end (B) is open (no connections with any strand):
					if not is_empty (strands_at_A) and is_empty (strands_at_B) then
						insert_mode := ATTACH_A;
					end if;

					-- CASE 3:
					-- The new segment ends (B) at an existing strand.
					-- The start (A) is open (no connections with any strand):
					if is_empty (strands_at_A) and not is_empty (strands_at_B) then
						insert_mode := ATTACH_B;
					end if;

					-- CASE 4:
					-- The new segment will be attached with A and B end.
					-- This includes two cases:
					-- CASE 4.a: The new segment connects segments that belong to
					--           the same strand.
					-- CASE 4.b: The new segment connects segments that belong to
					--           two different strands.
					if not is_empty (strands_at_A) and not is_empty (strands_at_B) then
						insert_mode := ATTACH_A_AND_B;
					end if;


					case insert_mode is
						when NEW_STRAND => -- CASE 1
							-- Create a new strand that contains the given segment:
							log (text => "Create new strand.", level => log_threshold + 3);
							create_new_strand;

						when ATTACH_A => -- CASE 2
							-- Take the first strand that has been located at
							-- the A end of the segment and insert the segment
							-- in that strand:
							log (text => "Attach A end of segment to strand.", level => log_threshold + 3);
							strand_cursor := first_element (strands_at_A);
							net.strands.update_element (strand_cursor, query_strand'access);

						when ATTACH_B => -- CASE 3
							-- Take the first strand that has been located at
							-- the B end of the segment and insert the segment
							-- in that strand:
							log (text => "Attach B end of segment to strand.", level => log_threshold + 3);
							strand_cursor := first_element (strands_at_B);
							net.strands.update_element (strand_cursor, query_strand'access);

						when ATTACH_A_AND_B => -- CASE 4
							attach_segment_at_both_ends;

					end case;
				end query_net;
			
		
			begin
				module.nets.update_element (net_cursor, query_net'access);
			end query_module;



		begin
			log (text => "Insert segment " & to_string (segment), level => log_threshold + 1);
			log_indentation_up;

			get_strands_at_AB;
			generic_modules.update_element (module_cursor, query_module'access);

			log_indentation_down;
		end query_segment;



	begin
		log (text => "module " & to_string (module_cursor) 
			 & " insert net segments" -- CS & to_string (segments)
			 & " in net " & get_net_name (net_cursor)
			 & " on sheet " & to_string (sheet),
			 level => log_threshold);

		log_indentation_up;


		-- Test existence of foreign nets here
		-- and reject all the given segments
		if segments_touch_foreign_net (module_cursor, net_cursor,
			sheet, segments, log_threshold + 1)
		then
			log (text => "segments rejected due to foreign net segments.", level => log_threshold);
		else
			-- Iterate through the given segments:
			segments.iterate (query_segment'access);
		end if;

		log_indentation_down;
	end insert_net_segments;



	


	procedure insert_net_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		sheet			: in type_sheet;
		segment			: in type_net_segment;
		log_threshold	: in type_log_level)
	is
		
		-- The segment to be inserted can run across the ends of
		-- other already existing strands. The ends of the existing
		-- segments lie between the A and B end of the segment to be inserted.
		-- So we need a list of the strands and segments that end between
		-- A and B of the segment to be inserted:
		use pac_strand_segment_cursors;
		strands_between_AB : pac_strand_segment_cursors.list;

		new_segments : pac_net_segments.list;

		-- This procedure searches for strands (of the given net)
		-- that start or end between A and B of the given segment:
		procedure get_strands_between_AB is 
			strand_count : count_type;
			fragment_count : natural;
		begin
			strands_between_AB := get_strands (
				module_cursor, net_cursor, segment, sheet, log_threshold + 1);

			strand_count := strands_between_AB.length;

			fragment_count := 1 + natural (strand_count);

			log (text => "Strands found: " & count_type'image (strand_count),
				level => log_threshold + 1);


			case fragment_count is
				when 1 =>
					log (text => "The given segment will be inserted as it is.",
						level => log_threshold + 1);

					new_segments.append (segment);


				when others =>
					log (text => "The given segment will be split in " 
						& natural'image (fragment_count) & " segments.",
						level => log_threshold + 1);

					new_segments := split_segment (segment, strands_between_AB, log_threshold + 1);

			end case;

			insert_net_segments (module_cursor, net_cursor, sheet, new_segments, log_threshold + 2);
		end;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " insert net segment " & to_string (segment)
			 & " in net " & get_net_name (net_cursor)
			 & " on sheet " & to_string (sheet),
			 level => log_threshold);

		log_indentation_up;

		get_strands_between_AB;

		log_indentation_down;
	end insert_net_segment;

	


	
	
	procedure insert_net_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		A				: in type_object_position; -- sheet/x/y
		B				: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is		
		net_cursor : pac_nets.cursor;
		segment : type_net_segment;
	begin
		log (text => "module " & to_string (module_cursor) 
			& " insert net segment in net " & to_string (net_name) 
			& " on sheet " & to_string (get_sheet (A)),
			level => log_threshold);
		
		-- build the segment from given start and end point
		set_A (segment, A.place);
		set_B (segment, B);

		log (text => " segment: " & to_string (segment), level => log_threshold);

		log_indentation_up;
		
		-- The net can be in the module already. 
		-- Locate the requested net in the module.
		-- If the net does not exist yet, then net_cursor will
		-- be no_element:
		net_cursor := locate_net (module_cursor, net_name);

		if not has_element (net_cursor) then
			log (text => "Net " & to_string (net_name) & 
				 " does not exist yet and will be created.",
				 level => log_threshold + 1);

			declare
				created : boolean;
			begin
				create_net (
					module_cursor	=> module_cursor,
					net_name		=> net_name,
					created			=> created,
					net_cursor		=> net_cursor,
					log_threshold	=> log_threshold + 1);
			end;						
		end if;

		
		insert_net_segment (module_cursor, net_cursor,
			get_sheet (A), segment, log_threshold + 2);					

		update_strand_positions (module_cursor, log_threshold + 2);
		
		update_ratsnest (module_cursor, log_threshold + 2);
		
		log_indentation_down;		
	end insert_net_segment;


	



	

	procedure set_net_class (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_class		: in pac_net_class_name.bounded_string; -- pwr
		log_threshold	: in type_log_level)
	is
		net_cursor : pac_nets.cursor; -- points to the net

		
		procedure query_module (
			name	: in pac_module_name.bounded_string;
			module	: in out type_generic_module)
		is
			procedure set_class (
				name	: in pac_net_name.bounded_string;
				net		: in out type_net)
			is 
				use pac_net_class_name;
			begin
				if net.class = net_class then
					log (text => "Net already in class " 
							& enclose_in_quotes (et_net_class.to_string (net_class)),
						level => log_threshold + 1);
				else
					log (text => "Changing net class from "
						 & enclose_in_quotes (et_net_class.to_string (net.class)) 
						 & " to " & enclose_in_quotes (et_net_class.to_string (net_class)),
						level => log_threshold + 1);

					net.class := net_class;
				end if;
			end set_class;
			
		begin
			pac_nets.update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> set_class'access);

		end query_module;
								
	begin
		log (text => "module " & to_string (module_cursor)
			& " setting class of net " & enclose_in_quotes (to_string (net_name)) 
			& " to " & enclose_in_quotes (to_string (net_class)),
			level => log_threshold);
		

		-- The net can be in the module already. Locate the requested net in the module.
		-- net_cursor will point to no_element if the net is not already there.
		net_cursor := locate_net (module_cursor, net_name);

		log_indentation_up;

		-- CS test whether given net class exists
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
			
		log_indentation_down;
	end set_net_class;






	
	
	procedure set_scope (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in et_netlists.type_net_scope; -- local/global
		log_threshold	: in type_log_level) 
	is
		net_cursor : pac_nets.cursor; -- points to the net

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure set (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is begin
				net.scope := scope;
			end set;

			
		begin -- query_nets
			pac_nets.update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> set'access);

		end query_nets;

		
	begin -- set_scope
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) &
			" setting scope of net " & to_string (net_name) &
			" to" & et_netlists.to_string (scope),
			level => log_threshold);


		-- locate the net
		net_cursor := locate_net (module_cursor, net_name);

		if net_cursor /= pac_nets.no_element then

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_nets'access);

			-- CS update_ratsnest (module_cursor, log_threshold + 1)
		else
			net_not_found (net_name);
		end if;
	end set_scope;






	
	
	procedure place_junction (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in type_object_position; -- sheet/x/y, rotation doesn't matter
		log_threshold	: in type_log_level) 
	is
		segment_found : boolean := false; -- goes true if a net segment has been found to place the junction at

		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;
				
				procedure query_segments (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;
					old_segment : type_net_segment; -- here a backup of the old segment lives
					old_segment_orientation : type_line_orientation; -- horizontal, vertical, sloped

					
					procedure insert_two_new_segments is
						segment_1, segment_2 : type_net_segment;

						
						procedure update_labels is
							use pac_net_labels;

							procedure query_labels_horizontal (cursor : in pac_net_labels.cursor) is begin
								-- All labels left of place go into segment_1,
								-- whereas labels on the right go into segment_2:
								if get_x (element (cursor).position) < get_x (place) then
									append (segment_1.labels, element (cursor));
								else
									append (segment_2.labels, element (cursor));
								end if;									  
							end query_labels_horizontal;

							
							procedure query_labels_vertical (cursor : in pac_net_labels.cursor) is begin
								-- All labels below place go into segment_1,
								-- whereas labels above go into segment_2:
								if get_y (element (cursor).position) < get_y (place) then
									append (segment_1.labels, element (cursor));
								else
									append (segment_2.labels, element (cursor));
								end if;									  
							end query_labels_vertical;

							
						begin -- update_labels
							log (text => "updating net labels ...", level => log_threshold + 1);
							log_indentation_up;

							case old_segment_orientation is
								when ORIENT_HORIZONTAL =>
									iterate (old_segment.labels, query_labels_horizontal'access);

								when ORIENT_VERTICAL =>
									iterate (old_segment.labels, query_labels_vertical'access);

								when ORIENT_SLOPING => raise constraint_error; -- CS should never happen
							end case;
							log_indentation_down;
						end update_labels;

						
						-- Queries the positions of the device ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
						procedure update_device_ports is 
							use pac_device_ports;
							use et_symbols;

							
-- 							procedure query_ports (cursor : in pac_device_ports.cursor) is
-- 								device_name 	: type_device_name; -- IC23
-- 								port_name		: pac_port_name.bounded_string; -- CE
-- 								port_position 	: type_vector_model; -- the xy-position of the port
-- 							begin
-- 								device_name	:= element (cursor).device_name;
-- 								port_name	:= element (cursor).port_name;
-- 
-- 								-- locate the port by module, device and port name:
-- 								port_position := get_position (module_cursor, device_name, port_name, log_threshold + 1).place;
-- 								log_indentation_up;
-- 								
-- 								log (text => "device " & to_string (device_name) & " port " & to_string (port_name) &
-- 									" at" & to_string (port_position),
-- 									level => log_threshold + 1);
-- 
-- 								-- If the port was at the start point of the old segment, then
-- 								-- it goes into segment_1.
-- 								if port_position = get_A (old_segment) then
-- 									insert (segment_1.ports.devices, element (cursor));
-- 
-- 								-- If the port was at the end point of the old segment, then
-- 								-- it goes into segment_2.
-- 								elsif port_position = get_B (old_segment) then
-- 									insert (segment_2.ports.devices, element (cursor));
-- 
-- 								-- If port was somewhere else, we have a problem. This should never happen.
-- 								else
-- 									log (ERROR, "port not on segment !");
-- 									raise constraint_error;
-- 								end if;
-- 								
-- 								log_indentation_down;
-- 							end query_ports;
							
							
						begin -- update_device_ports
							log (text => "updating device ports ...", level => log_threshold + 1);
							log_indentation_up;
							
							-- CS iterate (old_segment.ports.devices, query_ports'access);
							log_indentation_down;
						end update_device_ports;

						
						-- Queries the positions of the submodule ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
						procedure update_submodule_ports is 
							use pac_submodule_ports;

							
-- 							procedure query_ports (cursor : in pac_submodule_ports.cursor) is
-- 								use et_schematic_ops.submodules;
-- 								submod_name 	: pac_module_instance_name.bounded_string; -- MOT_DRV_3
-- 								port_name		: pac_net_name.bounded_string; -- RESET
-- 								port_position 	: type_vector_model; -- the xy-position of the port
-- 							begin
-- 								submod_name	:= element (cursor).module_name; -- CLOCK_GENERATOR
-- 								port_name	:= element (cursor).port_name;	-- RESET
-- 
-- 								-- locate the port by module, submodule and port name:
-- 								port_position := get_submodule_port_position (module_name, submod_name, port_name, log_threshold + 1).place;
-- 								log_indentation_up;
-- 								
-- 								log (text => "submodule " & to_string (submod_name) & " port " & to_string (port_name) &
-- 									" at" & to_string (port_position),
-- 									level => log_threshold + 1);
-- 
-- 								-- If the port was at the start point of the old segment, then
-- 								-- it goes into segment_1.
-- 								if port_position = get_A (old_segment) then
-- 									insert (segment_1.ports.submodules, element (cursor));
-- 
-- 								-- If the port was at the end point of the old segment, then
-- 								-- it goes into segment_2.
-- 								elsif port_position = get_B (old_segment) then
-- 									insert (segment_2.ports.submodules, element (cursor));
-- 
-- 								-- If port was somewhere else, we have a problem. This should never happen.
-- 								else
-- 									log (ERROR, "port not on segment !");
-- 									raise constraint_error;
-- 								end if;
-- 								
-- 								log_indentation_down;
-- 							end query_ports;

							
						begin -- update_submodule_ports
							log (text => "updating submodule ports ...", level => log_threshold + 1);
							log_indentation_up;
							
							-- CS iterate (old_segment.ports.submodules, query_ports'access);
							log_indentation_down;
						end update_submodule_ports;

						
						-- Queries the positions of the netchanger ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
						procedure update_netchanger_ports is 
							use et_netlists;
							use et_netlists.pac_netchanger_ports;
							use et_submodules;

							
-- 							procedure query_ports (cursor : in pac_netchanger_ports.cursor) is
-- 								index			: type_netchanger_id; -- 1,2,3,...
-- 								port			: type_netchanger_port_name; -- SLAVE/MASTER
-- 								port_position 	: type_vector_model; -- the xy-position of the port
-- 							begin
-- 								index := element (cursor).index;
-- 								port := element (cursor).port;
-- 
-- 								-- locate the port by module, netchanger index and port:
-- 								port_position := et_schematic_ops.submodules.get_netchanger_port_position (
-- 									module_name, index, port, log_threshold + 1).place;
-- 								
-- 								log_indentation_up;
-- 								
-- 								log (text => "netchanger " & to_string (index) & " port " & to_string (port) &
-- 									" at" & to_string (port_position),
-- 									level => log_threshold + 1);
-- 
-- 								-- If the port was at the start point of the old segment, then
-- 								-- it goes into segment_1.
-- 								if port_position = get_A (old_segment) then
-- 									insert (segment_1.ports.netchangers, element (cursor));
-- 
-- 								-- If the port was at the end point of the old segment, then
-- 								-- it goes into segment_2.
-- 								elsif port_position = get_B (old_segment) then
-- 									insert (segment_2.ports.netchangers, element (cursor));
-- 
-- 								-- If port was somewhere else, we have a problem. This should never happen.
-- 								else
-- 									log (ERROR, "port not on segment !");
-- 									raise constraint_error;
-- 								end if;
-- 								
-- 								log_indentation_down;
-- 							end query_ports;

							
						begin -- update_netchanger_ports
							log (text => "updating netchanger ports ...", level => log_threshold + 1);
							log_indentation_up;
							
							-- CS iterate (old_segment.ports.netchangers, query_ports'access);
							log_indentation_down;
						end update_netchanger_ports;

						
					begin -- insert_two_new_segments
						-- set start and end points of new segments
						set_A (segment_1, get_A (old_segment));
						set_B (segment_1, get_place (place));
						set_A (segment_2, get_place (place));
						set_B (segment_2, get_B (old_segment));

						-- set junctions
						segment_1.junctions.A := old_segment.junctions.A;
						segment_1.junctions.B := true; -- because there is the new junction
						segment_2.junctions.A := false; -- no need for another junction at the same place
						segment_2.junctions.B := old_segment.junctions.B;

						-- Labels and ports which were part of the old segment
						-- must now be assigned to the two new segments.
						update_labels;
						update_device_ports;
						update_submodule_ports;
						update_netchanger_ports;
						
						pac_net_segments.insert (
							container	=> strand.segments,
							before		=> segment_cursor,
							new_item	=> segment_1);

						pac_net_segments.insert (
							container	=> strand.segments,
							before		=> segment_cursor,
							new_item	=> segment_2);
					end insert_two_new_segments;

					
					procedure junction_at_A (segment : in out type_net_segment) is begin
						segment.junctions.A := true;
					end;

					
					procedure junction_at_B (segment : in out type_net_segment) is begin
						segment.junctions.B := true;
					end;

					
				begin -- query_segments
					while segment_cursor /= pac_net_segments.no_element loop

						-- The junction can be placed at the start or end point of a segment OR
						-- between start and end point of a segment. If none of these conditions
						-- is positive, go to next segment.
						
						--log_indentation_up;
						--log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);

						if get_place (place) = get_A (segment_cursor) then

							-- place junction at start point of segment
							update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> junction_at_A'access);

							segment_found := true;
							exit; -- no need to search for other segments
							
						elsif get_place (place) = get_B (segment_cursor) then

							-- place junction at end point of segment
							update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> junction_at_B'access);

							segment_found := true;
							exit; -- no need to search for other segments
							
						elsif between_A_and_B (
							catch_zone	=> set_catch_zone (place.place, 0.0),
							segment		=> segment_cursor) 
						then -- targeted segment found

							log (text => "net " & to_string (net_name) & " strand" &
								 to_string (position => strand.position), level => log_threshold + 1);
							log (text => to_string (segment_cursor), level => log_threshold + 1);

							-- Backup properties of old segment (it provides information on labels, ports and junctions):
							old_segment := element (segment_cursor);
							old_segment_orientation := get_segment_orientation (segment_cursor);

							-- It is not allowed to place a junction in a sloped segment,
							-- because splitting sloping segments seems a rare, difficult and dangerous task.
							if old_segment_orientation = ORIENT_SLOPING then
								junction_in_sloping_segment (place);
							end if;
							
							-- delete the targeted segment. it will later be replaced by two new segments.
							delete (strand.segments, segment_cursor);

							-- Insert two new segments in the strand
							-- and rearrange the ports of devices, submodules and netchangers.
							insert_two_new_segments;
							
							-- no further search required
							segment_found := true; 
							exit;
						end if;
							
 						--log_indentation_down;
						next (segment_cursor);
					end loop;

				end query_segments;

				
			begin -- query_strands
				while (not segment_found) and strand_cursor /= pac_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if get_sheet (element (strand_cursor).position) = get_sheet (place) then
						--log ("net " & to_string (key (net_cursor)), log_threshold + 1);
						--log_indentation_up;
						
						--log (text => "strand " & to_string (position => element (strand_cursor).position),
						--	log_threshold + 1);
					
						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
						--log_indentation_down;
					end if;

					next (strand_cursor);
				end loop;
			end query_strands;

			
		begin -- query_nets
			while (not segment_found) and net_cursor /= pac_nets.no_element loop

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;


		use pac_net_names;
		nets_at_place : pac_net_names.list;
		
		
	begin -- place_junction
		log (text => "module " & to_string (module_cursor) 
			& " placing junction at" & to_string (position => place) 
			& " ...", 
			level => log_threshold);
		
		log_indentation_up;
		

		-- Figure out how many nets are crossing the given place:
		nets_at_place := get_nets_at_place (module_cursor, place, log_threshold + 1);

		
		case length (nets_at_place) is
			when 0 =>
				log (WARNING, "Attempt to place junction in the void rejected !");

			when 1 =>
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> query_nets'access);

			
				update_ratsnest (module_cursor, log_threshold + 1);

			when others =>
				raise semantic_error_1 with
					"ERROR: Attempt to connect different nets via a junction rejected !";
		end case;
		
		log_indentation_down;
	end place_junction;
	


	

	function to_string (
		object	: in type_object_label)
		return string
	is begin
		return "net " & get_net_name (object.net_cursor)
			& " simple label at " & get_position (object.label_cursor);
			-- CS other properties ?
	end;

	
	
	function to_string (
		object	: in type_object_label_tag)
		return string
	is begin
		return "net " & get_net_name (object.net_cursor)
			& " tag label"; 
		-- CS Strand " & get_position (object.strand_cursor)
		-- & " segment " & to_string (object.segment_cursor)
		-- & to_string (object.start_end) & " point";
		-- CS direction, rotation
	end;

	
	

	procedure reset_labels (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (
					strand : in out type_strand)
				is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (
						segment : in out type_net_segment)
					is 
						label_cursor : pac_net_labels.cursor := segment.labels.first;

						procedure query_label (label : in out type_net_label_simple) is begin
							log (text => "label: " & get_position (label), level => log_threshold + 3);
							reset_status (label);
						end;
						
					begin
						log (text => "segment: " & to_string (segment), level => log_threshold + 2);

						-- Iterate the labels of the segment:
						while has_element (label_cursor) loop
							log_indentation_up;
							segment.labels.update_element (label_cursor, query_label'access);
							log_indentation_down;
							next (label_cursor);
						end loop;	

						-- Reset status of tag labels:
						reset_status (segment.tag_labels);
					end query_segment;
					
						
				begin
					-- Iterate the segments of the strand:
					while has_element (segment_cursor) loop
						strand.segments.update_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				while has_element (strand_cursor) loop
					net.strands.update_element (strand_cursor, query_strand'access);
					next (strand_cursor);
				end loop;
			end query_net;

			
		begin
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " reset all net labels.",
			level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_labels;



	


	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		label			: in type_object_label;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is

				procedure query_strand (strand : in out type_strand) is

					procedure query_segment (segment : in out type_net_segment) is 

						procedure query_label (label : in out type_net_label_simple) is begin
							modify_status (label, operation);
						end;
							
					begin
						segment.labels.update_element (label.label_cursor, query_label'access);						
					end query_segment;
					
				begin
					strand.segments.update_element (label.segment_cursor, query_segment'access);
				end query_strand;
				
			begin
				net.strands.update_element (label.strand_cursor, query_strand'access);
			end query_net;
			
		begin
			module.nets.update_element (label.net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of simple net label "
			& get_net_name (label.net_cursor) 
			& " strand " & get_position (label.strand_cursor)
			& " " & to_string (label.segment_cursor)
			& " " & get_position (label.label_cursor)
			-- CS use to_string (label) -- see functin to_string above
			& " / " & to_string (operation),
			level => log_threshold);


		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;





	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		label			: in type_object_label_tag;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is

				procedure query_strand (strand : in out type_strand) is

					procedure query_segment (segment : in out type_net_segment) is begin
						case label.start_end is
							when A =>
								modify_status (segment.tag_labels.A, operation);

							when B =>
								modify_status (segment.tag_labels.B, operation);
						end case;
					end query_segment;
					
				begin
					strand.segments.update_element (label.segment_cursor, query_segment'access);
				end query_strand;
				
			begin
				net.strands.update_element (label.strand_cursor, query_strand'access);
			end query_net;
			
		begin
			module.nets.update_element (label.net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of " & to_string (label)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;

	


	

	procedure propose_labels (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				strand_cursor : pac_strands.cursor := net.strands.first;

			
				procedure query_strand (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (segment : in out type_net_segment) is
						label_cursor : pac_net_labels.cursor := segment.labels.first;


						procedure query_label (label : in out type_net_label_simple) is begin
							if in_catch_zone (catch_zone, get_position (label)) then
								log (text => "in catch zone", level => log_threshold + 5);
								set_proposed (label);
								count := count + 1;
							end if;
						end query_label;
							
							
					begin
						-- Iterate through the labels:
						while has_element (label_cursor) loop
							log (text => get_position (label_cursor), level => log_threshold + 4);
							log_indentation_up;
							segment.labels.update_element (label_cursor, query_label'access);
							log_indentation_down;
							next (label_cursor);
						end loop;
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) loop
						log (text => to_string (segment_cursor), level => log_threshold + 3);
						log_indentation_up;
						strand.segments.update_element (segment_cursor, query_segment'access);
						log_indentation_down;
						next (segment_cursor);
					end loop;
				end query_strand;
				
				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) loop
					if get_sheet (strand_cursor) = active_sheet then
						log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
						log_indentation_up;
						net.strands.update_element (strand_cursor, query_strand'access);
						log_indentation_down;
					end if;
					next (strand_cursor);
				end loop;				
			end query_net;
			
	
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;
			
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " proposing net labels in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end propose_labels;






	procedure propose_labels_tag (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				strand_cursor : pac_strands.cursor := net.strands.first;

			
				procedure query_strand (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (segment : in out type_net_segment) is begin
						-- Probe the A end of the segment:
						if in_catch_zone (catch_zone, get_A (segment)) then
							set_proposed (segment.tag_labels.A);
							count := count + 1;
							log (text => "A end" & to_string (get_A (segment)), level => log_threshold + 4);
						end if;

						-- Probe the B end of the segment:
						if in_catch_zone (catch_zone, get_B (segment)) then
							set_proposed (segment.tag_labels.B);
							count := count + 1;
							log (text => "B end" & to_string (get_B (segment)), level => log_threshold + 4);
						end if;
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) loop
						log (text => to_string (segment_cursor), level => log_threshold + 3);
						log_indentation_up;
						strand.segments.update_element (segment_cursor, query_segment'access);
						log_indentation_down;
						next (segment_cursor);
					end loop;
				end query_strand;
				
				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) loop
					if get_sheet (strand_cursor) = active_sheet then
						log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
						log_indentation_up;
						net.strands.update_element (strand_cursor, query_strand'access);
						log_indentation_down;
					end if;
					next (strand_cursor);
				end loop;				
			end query_net;
			
	
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				module.nets.update_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;
			
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " proposing tag net labels in " & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end propose_labels_tag;



	
	


	function get_first_label (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_label
	is
		result : type_object_label;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			proceed : boolean := true;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (strand : in type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;


					procedure query_segment (segment : in type_net_segment) is
						label_cursor : pac_net_labels.cursor := segment.labels.first;

						
						procedure query_label (label : in type_net_label_simple) is

							
							procedure set_result is begin
								result.net_cursor		:= net_cursor;
								result.strand_cursor	:= strand_cursor;
								result.segment_cursor	:= segment_cursor;
								result.label_cursor		:= label_cursor;
								log (text => "match: " & to_string (result), level => log_threshold + 3);
								proceed := false; -- no further probing required
							end set_result;

							
						begin
							case flag is
								when PROPOSED =>
									if is_proposed (label) then
										set_result;
									end if;
				
								when SELECTED =>
									if is_selected (label) then
										set_result;
									end if;
				
								when others => null; -- CS
							end case;
						end query_label;

						
					begin
						-- Iterate through the labels:
						while has_element (label_cursor) and proceed loop
							log (text => get_position (label_cursor), level => log_threshold + 3);
							log_indentation_up;
							query_element (label_cursor, query_label'access);
							log_indentation_down;
							next (label_cursor);
						end loop;
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) and proceed loop
						log (text => to_string (segment_cursor), level => log_threshold + 2);
						log_indentation_up;
						query_element (segment_cursor, query_segment'access);
						log_indentation_down;
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) and proceed loop
					log (text => "strand " & get_position (strand_cursor), level => log_threshold + 1);
					log_indentation_up;
					query_element (strand_cursor, query_strand'access);
					log_indentation_down;
					next (strand_cursor);
				end loop;
			end query_net;

			
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) and proceed loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first simple net label / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		
		return result;
	end get_first_label;





	

	function get_first_label_tag (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_label_tag
	is
		result : type_object_label_tag;


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			proceed : boolean := true;


			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (strand : in type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;


					procedure query_segment (segment : in type_net_segment) is
						
						start_end : type_start_end_point; -- A or B end
						
						
						procedure query_label (label : in type_net_label_tag) is

							procedure set_result is begin
								result.net_cursor		:= net_cursor;
								result.strand_cursor	:= strand_cursor;
								result.segment_cursor	:= segment_cursor;
								result.start_end		:= start_end;
								log (text => "match: " & to_string (result), level => log_threshold + 3);
								proceed := false; -- no further probing required
							end set_result;

							
						begin
							case flag is
								when PROPOSED =>
									if is_proposed (label) then
										set_result;
									end if;
				
								when SELECTED =>
									if is_selected (label) then
										set_result;
									end if;
				
								when others => null; -- CS
							end case;
						end query_label;
							
						
					begin
						-- Probe the A end of the segment:
						start_end := A;
						log (text => "A end at " & to_string (get_A (segment)), level => log_threshold + 3);
						query_label (segment.tag_labels.A);

						-- Probe the B end of the segment if nothing found at A end:
						if proceed then
							start_end := B;
							log (text => "B end at " & to_string (get_B (segment)), level => log_threshold + 3);
							query_label (segment.tag_labels.B);
						end if;
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) and proceed loop
						log (text => to_string (segment_cursor), level => log_threshold + 2);
						log_indentation_up;
						query_element (segment_cursor, query_segment'access);
						log_indentation_down;
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) and proceed loop
					log (text => "strand " & get_position (strand_cursor), level => log_threshold + 1);
					log_indentation_up;
					query_element (strand_cursor, query_strand'access);
					log_indentation_down;
					next (strand_cursor);
				end loop;
			end query_net;

			
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) and proceed loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first tag net label / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		
		return result;
	end get_first_label_tag;



	
	
	
	procedure place_net_label_simple (
		module_cursor	: in pac_generic_modules.cursor;
		segment_position: in type_object_position; -- sheet/x/y
		label_position	: in type_vector_model := origin; -- x/y
		rotation		: in et_schematic_coordinates.type_rotation_model := zero_rotation; -- 0, 90, 180. Relevant for simple labels only.
		log_threshold	: in type_log_level) 
	is
		net_cursor : pac_nets.cursor; -- points to the net

		
		-- function no_label_placed return string is begin
		-- 	return (et_schematic_coordinates.to_string (position => segment_position) & " !" &
		-- 		" No label placed ! Specify another position and try again.");
		-- end;
		
		use pac_net_names;
		nets : pac_net_names.list;
		net_name : pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- This flag goes true once the first segment of the 
			-- targeted net at the targeted sheet has been found.
			segment_found : boolean := false;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is				
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (segment : in out type_net_segment) is 
						use pac_net_labels;
						label : type_net_label_simple;
					begin
						-- label_position is relative to segment_position
						label.position := label_position;
						move_by (label.position, segment_position.place);
						-- now label.position is absolute

						-- snap given rotation to either 0 or 90 degree
						label.rotation := snap (rotation);
						
						segment.labels.append (label);								
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) and not segment_found loop

						-- If the given target position is on the
						-- candidate segment, then the right segment has
						-- been found:
						if on_line (
							point 	=> segment_position.place,
							line	=> element (segment_cursor)) then
							
							strand.segments.update_element (segment_cursor, query_segment'access);

							-- signal iterations in upper level to cancel
							segment_found := true;
						end if;
						
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) and not segment_found loop
					
					-- We pick out only the strands on the targeted sheet:
					if get_sheet (strand_cursor) = get_sheet (segment_position) then
						net.strands.update_element (strand_cursor, query_strand'access);					
					end if;
					
					next (strand_cursor);
				end loop;
			end query_net;

			
		begin
			module.nets.update_element (net_cursor, query_net'access);
		end query_module;

		
	begin -- place_net_label
		log (text => "module " & to_string (module_cursor) &
			" place simple label next to segment at"  &
			to_string (position => segment_position) &
			to_string (label_position) &
			" rotation" & to_string (rotation),
			level => log_threshold);
		
		log_indentation_up;


		-- collect names of nets that cross the given segment_position
		nets := get_nets_at_place (module_cursor, segment_position, log_threshold + 1);

		log_indentation_up;
		
		
		case length (nets) is
			when 0 =>
				log (WARNING, "No net found at " & to_string (segment_position), level => log_threshold + 1);

			when 1 => 
				net_name := element (nets.first);
				log (text => "Found net: " & to_string (net_name), level => log_threshold + 1);
				
				-- Set the cursor to the net.
				net_cursor := locate_net (module_cursor, net_name);
				--log (text => "net name " & to_string (key (net_cursor)), level => log_threshold + 1);				
				generic_modules.update_element (module_cursor, query_module'access);

			when others =>
				log (WARNING, "More than one net found at" & to_string (segment_position), level => log_threshold + 1);
				-- CS show the net names
				
		end case;
		
		log_indentation_down;		
		log_indentation_down;
	end place_net_label_simple;





	procedure place_net_label_tag (
		module_cursor	: in pac_generic_modules.cursor;
		position		: in type_object_position; -- sheet/x/y
		direction		: in type_net_label_direction; -- INPUT, OUTPUT, PASSIVE, ...
		log_threshold	: in type_log_level) 
	is
		net_cursor : pac_nets.cursor; -- points to the net

		
		-- function no_label_placed return string is begin
		-- 	return (et_schematic_coordinates.to_string (position => position) & " !" &
		-- 		" No label placed ! Specify another position and try again.");
		-- end;
		
		use pac_net_names;
		nets : pac_net_names.list;
		net_name : pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- This flag goes true once the first segment of the 
			-- targeted net at the targeted sheet has been found.
			segment_found : boolean := false;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is				
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_segment (segment : in out type_net_segment) is 
						stub : constant type_stub := query_stub (
							module_cursor, net_name, position, log_threshold + 1);

						t : type_net_label_tag;
					begin
						-- A tag label can be attached to a stub only.
						-- So we test wheter the given position matches either
						-- the A or B end of the segment:
						if stub.is_stub then
							log (text => "stub found", level => log_threshold + 1);
							log_indentation_up;

							
							if get_place (position) = get_A (segment) then
								-- Enable the tag label on the A end:
								log (text => "attach label to A end of segment", level => log_threshold + 2);
								
								t := (active => true, direction => direction, others => <>); -- CS size, ...
								segment.tag_labels.A := t;

								-- Signal iterations in upper level to cancel:
								segment_found := true;						
							end if;


							if get_place (position) = get_B (segment) then
								-- Enable the tag label on the B end:
								log (text => "attach label to B end of segment", level => log_threshold + 2);
								
								t := (active => true, direction => direction, others => <>); -- CS size, ...
								segment.tag_labels.B := t;

								-- Signal iterations in upper level to cancel:
								segment_found := true;						
							end if;

							log_indentation_down;
							
						else
							--log (WARNING, "Net has no stub at" & to_string (position), console => true);
							log (text => "No stub found. No label placed.", level => log_threshold + 1);
						end if;

					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) and not segment_found loop	
						strand.segments.update_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;
				end query_strand;

				
			begin
				-- Iterate through the strands:
				while has_element (strand_cursor) and not segment_found loop
					
					-- We pick out only the strands on the targeted sheet:
					if get_sheet (strand_cursor) = get_sheet (position) then
						net.strands.update_element (strand_cursor, query_strand'access);					
					end if;
					
					next (strand_cursor);
				end loop;
			end query_net;

			
		begin
			module.nets.update_element (net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " place tag label at " & to_string (position => position)
			& " direction " & to_string (direction),
			level => log_threshold);
		
		log_indentation_up;


		-- Collect names of nets that cross the given target position:
		nets := get_nets_at_place (module_cursor, position, log_threshold + 1);

		log_indentation_up;
		

		case length (nets) is
			when 0 =>
				log (WARNING, "No net found at" & to_string (position), level => log_threshold + 1);

			when 1 => 
				net_name := element (nets.first);
				log (text => "Found net: " & to_string (net_name), level => log_threshold + 1);
				
				-- Set the cursor to the net.
				net_cursor := locate_net (module_cursor, net_name);
				--log (text => "net name " & to_string (key (net_cursor)), level => log_threshold + 1);
				generic_modules.update_element (module_cursor, query_module'access);

			when others =>
				log (WARNING, "More than one net found at" & to_string (position), level => log_threshold + 1);
				-- CS show the net names
				
		end case;
		
		log_indentation_down;
		log_indentation_down;
	end place_net_label_tag;






	
	
	procedure delete_net_label (
		module_cursor	: in pac_generic_modules.cursor;
		position		: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level) 
	is
		-- This flag goes true once the targeted net label
		-- has been found. All iterations are cancelled as soon as it goes true.
		label_found : boolean := false;

		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_segments (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_labels (segment : in out type_net_segment) is 
						use pac_net_labels;
						label_cursor : pac_net_labels.cursor := segment.labels.first;
					begin
						while label_cursor /= pac_net_labels.no_element loop

							-- If label sits at position, delete it from the label list
							-- of that segment:
							if element (label_cursor).position = position.place then
								delete (segment.labels, label_cursor);
								label_found := true;
								exit;
							end if;

							next (label_cursor);
						end loop;
					end query_labels;

					
				begin -- query_segments
					while not label_found and segment_cursor /= pac_net_segments.no_element loop

						update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> query_labels'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;

				
			begin -- query_strands
				while not label_found and strand_cursor /= pac_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if get_sheet (element (strand_cursor).position) = get_sheet (position) then

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
					end if;
					
					next (strand_cursor);
				end loop;
			end query_strands;

			
		begin -- query_nets
			while not label_found and net_cursor /= pac_nets.no_element loop
				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

		
	begin -- delete_net_label
		log (text => "module " & to_string (module_cursor) &
			" deleting net label at" &
			et_schematic_coordinates.to_string (position => position),
			level => log_threshold);
		
		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_nets'access);

		if not label_found then
			log (WARNING, "no net label found at given position !");
		end if;
		
		log_indentation_down;
	end delete_net_label;
	




	

	function query_stub (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		position		: in type_object_position; -- sheet/x/y		
		log_threshold	: in type_log_level)
		return type_stub 
	is
		net_cursor : pac_nets.cursor; -- points to the net
		
		ports : type_ports;
		
		stub_found : boolean := false;
		direction : type_stub_direction;

		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net) 
		is			
			strand_cursor : pac_strands.cursor := net.strands.first;

			
			procedure query_segments (strand : in type_strand) is
				segment_cursor : pac_net_segments.cursor := strand.segments.first;

				segment_counter : natural := 0;

				
				procedure probe_direction is
					-- Get the stub direction. If the segment is sloped then it
					-- does not qualify as stub.
					s : constant type_stub := stub_direction (segment_cursor, position.place);
				begin
					-- get stub direction
					if s.is_stub then -- stub is horizontal or vertical
						direction := s.direction;
						stub_found := true;
					end if;

					-- count the match (regardless if it is a stub or not)
					segment_counter := segment_counter + 1;
				end probe_direction;

				
			begin -- query_segments
				while segment_cursor /= pac_net_segments.no_element loop
					
					-- The given position must be a start or end point of a segment,
					if get_A (segment_cursor) = position.place then
						log (text => "match with start point of a segment", level => log_threshold + 2);
						probe_direction;
						
					elsif get_B (segment_cursor) = position.place then
						log (text => "match with end point of a segment", level => log_threshold + 2);						
						probe_direction;

					end if;

					next (segment_cursor);
				end loop;

				-- After probing all segments of the strand there must have been found only one
				-- segment at the given position. In this case we have a stub. 
				-- If more segments have been found, it is a junction or a corner/bend. The flag
				-- stub_found would be reset.
				if segment_counter /= 1 then
					stub_found := false; -- reset flag
				end if;
			end query_segments;

			
		begin -- query_strands
			while strand_cursor /= pac_strands.no_element loop
				
				-- We are interested in strands on the given sheet only:
				if get_sheet (element (strand_cursor).position) = get_sheet (position) then

					query_element (
						position	=> strand_cursor,
						process		=> query_segments'access);
				end if;

				-- exit this loop once a stub has been found
				if stub_found then exit; end if;
				
				next (strand_cursor);
			end loop;
		end query_strands;

		
	begin -- query_stub
		log (text => "querying stub at" & to_string (position), level => log_threshold);

		-- Query ports at the given position.
		ports := get_ports (module_cursor, position, log_threshold + 1);
		
		-- If there are no ports then examine the net further.
		-- If there are devices, submodule or netchanger ports, then the given position
		-- is definitely not a stub.
		if no_ports (ports) then
			log (text => "no ports here. examining net further ...", level => log_threshold + 1);
		
			-- locate the net (it should exist)
			net_cursor := locate_net (module_cursor, net_name);
			
			query_element (
				position	=> net_cursor,
				process		=> query_strands'access);

			if not stub_found then
				return (is_stub => false);
			else
				-- put_line (type_stub_direction'image (direction));
				return (is_stub => true, direction => direction);
			end if;

		else -- means there are ports at the given position
			return (is_stub => false);
		end if;

	end query_stub;


	



	procedure show_label_simple (
		module_cursor	: in pac_generic_modules.cursor;
		label			: in type_object_label;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 

				procedure query_strand (strand : in out type_strand) is

					procedure query_segment (segment : in out type_net_segment) is 

						procedure query_label (label : in out type_net_label_simple) is begin
							set_selected (label);
						end;

					begin
						segment.labels.update_element (label.label_cursor, query_label'access);
					end query_segment;
					
				begin
					strand.segments.update_element (label.segment_cursor, query_segment'access);
				end;

			begin
				net.strands.update_element (label.strand_cursor, query_strand'access);
			end query_net;
			
		begin
			module.nets.update_element (label.net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " showing/highlight simple label "
			& to_string (label),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end show_label_simple;
		

	

	

	
	procedure show_label_tag (
		module_cursor	: in pac_generic_modules.cursor;
		label			: in type_object_label_tag;
		log_threshold	: in type_log_level)
	is


		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is			

			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 

				procedure query_strand (strand : in out type_strand) is

					procedure query_segment (segment : in out type_net_segment) is begin
						case label.start_end is
							when A => set_selected (segment.tag_labels.A);
							when B => set_selected (segment.tag_labels.B);
						end case;
					end query_segment;
					
				begin
					strand.segments.update_element (label.segment_cursor, query_segment'access);
				end;

				
			begin
				net.strands.update_element (label.strand_cursor, query_strand'access);
			end query_net;

			
		begin
			module.nets.update_element (label.net_cursor, query_net'access);
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " showing/highlight tag label "
			& to_string (label),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end show_label_tag;


	
------------------------------------------------------------------------------------------

-- OBJECTS:

	
	function to_string (
		object_cursor : in pac_objects.cursor)
		return string
	is
		object : type_object := element (object_cursor);
	begin
		case object.cat is
			when CAT_VOID =>
				return "void";

			when CAT_SEGMENT =>
				return to_string (object.segment);

			when CAT_STRAND =>
				return to_string (object.strand);
				
			when CAT_NET =>
				return to_string (object.net);

			when CAT_LABEL =>
				return to_string (object.label);

			when CAT_LABEL_TAG =>
				return to_string (object.label_tag);
				
		end case;
	end to_string;
	

	

	
	function get_count (
		objects : in pac_objects.list)
		return natural
	is begin
		return natural (objects.length);
	end get_count;
	
	


	function get_net (
		object_cursor : in pac_objects.cursor)
		return pac_nets.cursor
	is
		object : type_object := element (object_cursor);

		c : pac_nets.cursor := pac_nets.no_element;		
	begin
		case object.cat is
			when CAT_SEGMENT =>
				return object.segment.net_cursor;

			when CAT_STRAND =>
				return object.strand.net_cursor;
				
			when CAT_NET =>
				return object.net.net_cursor;

			when others =>
				return c; -- CS exception ?
		end case;
	end;


	
	function get_strand (
		object_cursor : in pac_objects.cursor)
		return pac_strands.cursor
	is
		object : type_object := element (object_cursor);
	begin
		return object.segment.strand_cursor;
	end;
	


	function get_segment (
		object_cursor : in pac_objects.cursor)
		return pac_net_segments.cursor
	is
		object : type_object := element (object_cursor);
	begin
		return object.segment.segment_cursor;
	end;



	


	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object
	is
		result_category : type_object_category := CAT_VOID;
		result_segment 	: type_object_segment;		
		result_strand	: type_object_strand;
		result_net		: type_object_net;
		result_label	: type_object_label; -- CS rename to result_label_simple
		result_label_tag: type_object_label_tag;

	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		

		-- SEARCH FOR THE FIRST NET:
		
		-- If a net has been found, then go to the end of this procedure:
		result_net := get_first_net (module_cursor, flag, log_threshold + 1);

		if has_element (result_net.net_cursor) then
			-- A net has been found.
			-- CS log ?
			result_category := CAT_NET;
		end if;
		
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;




		-- SEARCH FOR THE FIRST STRAND::
		
		-- If a strand has been found, then go to the end of this procedure:
		result_strand := get_first_strand (module_cursor, flag, log_threshold + 1);

		if has_element (result_strand.net_cursor) then
			-- A strand has been found.
			-- CS log ?
			result_category := CAT_STRAND;
		end if;
		
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;




		
		
		-- SEARCH FOR THE FIRST NET SEGMENT:
		
		-- If a segment has been found, then go to the end of this procedure:
		result_segment := get_first_segment (module_cursor, flag, log_threshold + 1);

		if has_element (result_segment.segment_cursor) then
			-- A segment has been found.
			-- CS log ?
			result_category := CAT_SEGMENT;
		end if;

		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;



		
		

		-- SEARCH FOR THE FIRST SIMPLE NET LABEL:
		
		-- If a label has been found, then go to the end of this procedure:
		result_label := get_first_label (module_cursor, flag, log_threshold + 1);

		if has_element (result_label.label_cursor) then
			-- A label has been found.
			-- CS log ?
			result_category := CAT_LABEL;
		end if;
		
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;


		


		-- SEARCH FOR THE FIRST TAG NET LABEL:
		
		-- If a label has been found, then go to the end of this procedure:
		result_label_tag := get_first_label_tag (module_cursor, flag, log_threshold + 1);

		if has_element (result_label_tag.net_cursor) then
			-- A label has been found.
			-- CS log ?
			result_category := CAT_LABEL_TAG;
		end if;
		
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;


		
		
		-- If nothing has been found then the category is CAT_VOID.


	<<end_of_search>>
		
		log_indentation_down;
		
		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_SEGMENT =>
				return (CAT_SEGMENT, result_segment);

			when CAT_STRAND =>
				return (CAT_STRAND, result_strand);

			when CAT_NET =>
				return (CAT_NET, result_net);

			when CAT_LABEL =>
				return (CAT_LABEL, result_label);

			when CAT_LABEL_TAG =>
				return (CAT_LABEL_TAG, result_label_tag);
				
		end case;
	end get_first_object;





	
	function get_objects (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return pac_objects.list
	is
		use pac_objects;

		-- Here the objects are collected:
		result : pac_objects.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is


			procedure search_nets is
				net_cursor : pac_nets.cursor := module.nets.first;
				

				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in type_net) 
				is 

					-- This procedure appends the matching
					-- net cursor to the result:
					procedure collect is begin
						
						result.append ((
							cat	=> CAT_NET,
							net	=> (net_cursor => net_cursor)));

					end collect;
							
					
				begin
					case flag is
						when PROPOSED =>
							if is_proposed (net) then
								collect;
							end if;
	
						when SELECTED =>
							if is_selected (net) then
								collect;
							end if;
	
						when others => null; -- CS
					end case;
				end query_net;

				
			begin
				log (text => "nets (whole nets)", level => log_threshold + 1);
				log_indentation_up;
				
				-- Iterate through the nets:
				while has_element (net_cursor) loop
					log (text => "net " & get_net_name (net_cursor), level => log_threshold + 2);
					log_indentation_up;
					query_element (net_cursor, query_net'access);
					log_indentation_down;
					next (net_cursor);
				end loop;

				log_indentation_down;				
			end search_nets;

			----------------------------------------------------------------------------------------
			
			
			procedure search_strands is
				net_cursor : pac_nets.cursor := module.nets.first;
				

				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in type_net) 
				is 
					strand_cursor : pac_strands.cursor := net.strands.first;

					
					procedure query_strand (strand : in type_strand) is

						-- This procedure appends the matching
						-- net and strand cursor to the result:
						procedure collect is begin							
							result.append ((
								cat		=> CAT_STRAND,
								strand	=> (net_cursor, strand_cursor)));
						end collect;
							
					begin
						case flag is
							when PROPOSED =>
								if is_proposed (strand) then
									collect;
								end if;
		
							when SELECTED =>
								if is_selected (strand) then
									collect;
								end if;
		
							when others => null; -- CS
						end case;						
					end query_strand;
			
					
				begin
					-- Iterate through the strands:
					while has_element (strand_cursor) loop
						log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
						log_indentation_up;
						query_element (strand_cursor, query_strand'access);
						log_indentation_down;
						next (strand_cursor);
					end loop;
				end query_net;

				
			begin
				log (text => "net segments", level => log_threshold + 1);
				log_indentation_up;
				
				-- Iterate through the nets:
				while has_element (net_cursor) loop
					log (text => "net " & get_net_name (net_cursor), level => log_threshold + 2);
					log_indentation_up;
					query_element (net_cursor, query_net'access);
					log_indentation_down;
					next (net_cursor);
				end loop;

				log_indentation_down;				
			end search_strands;
		
			

			----------------------------------------------------------------------------------------
			
			
			procedure search_net_segments is
				net_cursor : pac_nets.cursor := module.nets.first;
				

				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in type_net) 
				is 
					strand_cursor : pac_strands.cursor := net.strands.first;

					
					procedure query_strand (strand : in type_strand) is
						segment_cursor : pac_net_segments.cursor := strand.segments.first;					


						procedure query_segment (seg : in type_net_segment) is

							-- This procedure appends the matching
							-- net, strand and segment cursor to the result:
							procedure collect is begin
								-- log (text => get_unit_name (unit_cursor), level => log_threshold + 4);
								
								result.append ((
									cat		=> CAT_SEGMENT,
									segment	=> (net_cursor, strand_cursor, segment_cursor)));

							end collect;
							
							
						begin
							case flag is
								when PROPOSED =>
									if is_proposed (seg) then
										collect;
									end if;
			
								when SELECTED =>
									if is_selected (seg) then
										collect;
									end if;
			
								when others => null; -- CS
							end case;
						end query_segment;

						
					begin
						-- Iterate through the segments:
						while has_element (segment_cursor) loop
							log (text => "segment " & to_string (segment_cursor), level => log_threshold + 3);
							log_indentation_up;
							query_element (segment_cursor, query_segment'access);
							log_indentation_down;
							next (segment_cursor);
						end loop;
					end query_strand;
			
					
				begin
					-- Iterate through the strands:
					while has_element (strand_cursor) loop
						log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
						log_indentation_up;
						query_element (strand_cursor, query_strand'access);
						log_indentation_down;
						next (strand_cursor);
					end loop;
				end query_net;

				
			begin
				log (text => "net segments", level => log_threshold + 1);
				log_indentation_up;
				
				-- Iterate through the nets:
				while has_element (net_cursor) loop
					log (text => "net " & get_net_name (net_cursor), level => log_threshold + 2);
					log_indentation_up;
					query_element (net_cursor, query_net'access);
					log_indentation_down;
					next (net_cursor);
				end loop;

				log_indentation_down;				
			end search_net_segments;
		

			----------------------------------------------------------------------------------------
			

			procedure search_net_labels is
				net_cursor : pac_nets.cursor := module.nets.first;
				

				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in type_net) 
				is 
					strand_cursor : pac_strands.cursor := net.strands.first;

					
					procedure query_strand (strand : in type_strand) is
						segment_cursor : pac_net_segments.cursor := strand.segments.first;					


						procedure query_segment (seg : in type_net_segment) is
							label_cursor : pac_net_labels.cursor := seg.labels.first;


							procedure query_label (label : in type_net_label_simple) is

								-- This procedure appends the matching
								-- net, strand, segment and label cursor to the result:
								procedure collect is begin
									-- log (text => get_position (label_cursor), level => log_threshold + 4);
									
									result.append ((
										cat		=> CAT_LABEL,
										label	=> (net_cursor, strand_cursor, segment_cursor, label_cursor)));

								end collect;
								
							begin
								case flag is
									when PROPOSED =>
										if is_proposed (label) then
											collect;
										end if;
				
									when SELECTED =>
										if is_selected (label) then
											collect;
										end if;
				
									when others => null; -- CS
								end case;
							end query_label;
							
							
						begin
							-- Iterate through the net labels:
							while has_element (label_cursor) loop
								log (text => "label " & get_position (label_cursor), level => log_threshold + 4);
								log_indentation_up;
								query_element (label_cursor, query_label'access);
								log_indentation_down;
								next (label_cursor);
							end loop;
						end query_segment;

						
					begin
						-- Iterate through the segments:
						while has_element (segment_cursor) loop
							log (text => "segment " & to_string (segment_cursor), level => log_threshold + 3);
							log_indentation_up;
							query_element (segment_cursor, query_segment'access);
							log_indentation_down;
							next (segment_cursor);
						end loop;
					end query_strand;
			
					
				begin
					-- Iterate through the strands:
					while has_element (strand_cursor) loop
						log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
						log_indentation_up;
						query_element (strand_cursor, query_strand'access);
						log_indentation_down;
						next (strand_cursor);
					end loop;
				end query_net;
				
				
			begin
				log (text => "simple net labels", level => log_threshold + 1);
				log_indentation_up;

				-- Iterate through the nets:
				while has_element (net_cursor) loop
					log (text => "net " & get_net_name (net_cursor), level => log_threshold + 2);
					log_indentation_up;
					query_element (net_cursor, query_net'access);
					log_indentation_down;
					next (net_cursor);
				end loop;
				
				log_indentation_down;
			end search_net_labels;



			----------------------------------------------------------------------------------------
			

			procedure search_net_labels_tag is
				net_cursor : pac_nets.cursor := module.nets.first;
				

				procedure query_net (
					net_name	: in pac_net_name.bounded_string;
					net			: in type_net) 
				is 
					strand_cursor : pac_strands.cursor := net.strands.first;

					
					procedure query_strand (strand : in type_strand) is
						segment_cursor : pac_net_segments.cursor := strand.segments.first;					


						procedure query_segment (seg : in type_net_segment) is

							start_end : type_start_end_point;

							
							procedure query_label (label : in type_net_label_tag) is

								-- This procedure appends the matching
								-- net, strand, segment and end point to the result:
								procedure collect is begin
									log (text => to_string (start_end) & " end", level => log_threshold + 4);
									
									result.append ((
										cat			=> CAT_LABEL_TAG,
										label_tag	=> (net_cursor, strand_cursor, segment_cursor, start_end)));

								end collect;

								
							begin
								case flag is
									when PROPOSED =>
										if is_proposed (label) then
											collect;
										end if;
				
									when SELECTED =>
										if is_selected (label) then
											collect;
										end if;
				
									when others => null; -- CS
								end case;
							end query_label;
							
							
						begin
							start_end := A;
							query_label (seg.tag_labels.A);
							
							start_end := B;
							query_label (seg.tag_labels.B);
						end query_segment;

						
					begin
						-- Iterate through the segments:
						while has_element (segment_cursor) loop
							log (text => "segment " & to_string (segment_cursor), level => log_threshold + 3);
							log_indentation_up;
							query_element (segment_cursor, query_segment'access);
							log_indentation_down;
							next (segment_cursor);
						end loop;
					end query_strand;
			
					
				begin
					-- Iterate through the strands:
					while has_element (strand_cursor) loop
						log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
						log_indentation_up;
						query_element (strand_cursor, query_strand'access);
						log_indentation_down;
						next (strand_cursor);
					end loop;
				end query_net;
				
				
			begin
				log (text => "tag net labels", level => log_threshold + 1);
				log_indentation_up;

				-- Iterate through the nets:
				while has_element (net_cursor) loop
					log (text => "net " & get_net_name (net_cursor), level => log_threshold + 2);
					log_indentation_up;
					query_element (net_cursor, query_net'access);
					log_indentation_down;
					next (net_cursor);
				end loop;
				
				log_indentation_down;
			end search_net_labels_tag;
			
			
		begin
			search_nets; -- addresses whole nets		
			search_strands;
			search_net_segments;			
			search_net_labels; -- CS rename to search_net_labels_simple
			search_net_labels_tag;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up objects / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;

		return result;
	end get_objects;

	





	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " modifying status of object "
			& type_object_category'image (object.cat)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		case object.cat is
			when CAT_SEGMENT =>
				modify_status (module_cursor, object.segment, operation, log_threshold + 1);

			when CAT_STRAND =>
				modify_status (module_cursor, object.strand, operation, log_threshold + 1);
				
			when CAT_NET =>
				modify_status (module_cursor, object.net, operation, log_threshold + 1);

			when CAT_LABEL =>
				modify_status (module_cursor, object.label, operation, log_threshold + 1);

			when CAT_LABEL_TAG =>
				modify_status (module_cursor, object.label_tag, operation, log_threshold + 1);
				
			when CAT_VOID =>
				null; -- CS
		end case;

		log_indentation_down;
	end modify_status;







	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is 
		use pac_objects;
		object : constant type_object := element (object_cursor);
	begin
		modify_status (module_cursor, object, operation, log_threshold);
	end modify_status;

	
	


	procedure reset_proposed_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor) &
			" resetting proposed objects",
			level => log_threshold);

		log_indentation_up;
		reset_proposed_segments (module_cursor, log_threshold + 1);
		reset_strands (module_cursor, log_threshold + 1);
		reset_proposed_nets (module_cursor, log_threshold + 1);
		
		reset_labels (module_cursor, log_threshold + 1); -- simple and tag
		
		log_indentation_down;
	end reset_proposed_objects;










	procedure set_segments_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net)
			is 
				strand_cursor : pac_strands.cursor := net.strands.first;
				

				procedure query_strand (strand : in type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					-- Get the sheet where the candidate strand is:
					sheet : type_sheet := get_sheet (strand);
					
					-- This procedure and sets start or end points of net 
					-- segments which are connected with the given segment as "moving":
					procedure query_segment (seg : in type_net_segment) is
						position : type_object_position;
					begin
						if is_selected (seg) then
							log (text => " is selected", level => log_threshold + 3);

							-- Set segments which are connected with the start point
							-- of the candidate segment as "moving":
							position := to_position (get_A (seg), sheet);
							set_segments_moving (module_cursor, position, log_threshold + 4);

							-- Set segments which are connected with the end point
							-- of the candidate segment as "moving":
							position := to_position (get_B (seg), sheet);
							set_segments_moving (module_cursor, position, log_threshold + 4);
						end if;
					end query_segment;

					
				begin
					-- Iterate through the segments:
					while has_element (segment_cursor) loop
						log (text => to_string (segment_cursor), level => log_threshold + 3);
						log_indentation_up;
						query_element (segment_cursor, query_segment'access);
						log_indentation_down;
						next (segment_cursor);
					end loop;
				end query_strand;
				
										 
			begin				
				-- Iterate through the strands:
				while has_element (strand_cursor) loop
					log (text => "strand " & get_position (strand_cursor), level => log_threshold + 2);
					log_indentation_up;
					query_element (strand_cursor, query_strand'access);
					log_indentation_down;
					next (strand_cursor);
				end loop;
			end query_net;
			
			
		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				log (text => "net " & get_net_name (net_cursor), level => log_threshold + 1);
				log_indentation_up;
				query_element (net_cursor, query_net'access);
				log_indentation_down;
				next (net_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " set net segments (connected with selected segments) moving.",
			level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end set_segments_moving;



	

	
	procedure set_primary_segment_AB_moving (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor; -- must point to a net segment
		point_of_attack	: in type_vector_model;
		movable_test	: in boolean;
		granted			: in out boolean;
		log_threshold	: in type_log_level)
	is

		-- This function tests whether the given segment
		-- can be moved at the given end (A/B).
		-- It is required only in case the caller has requested
		-- a "movable test":
		function is_movable (AB : type_start_end_point) 
			return boolean 
		is begin
			if segment_is_movable (
				module_cursor, element (object_cursor).segment, AB, log_threshold + 1)
			then
				return true;
			else
				log (text => "Segment can not be moved at end " & to_string (AB),
					 level => log_threshold + 1);

				return false;
			end if;
		end;

	
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				

				procedure query_strand (strand : in out type_strand) is


					-- This procedure queries a segment and sets
					-- its A or B or both ends forcefully to "moving". It does not care
					-- about ports which might be connected with the segment.
					-- Use this procedure in case the caller of the main procedure
					-- does not request a "movable test":
					procedure query_segment_force (seg : in out type_net_segment) is
						use pac_objects;
						
						-- Find the zone at which the segment
						-- is being attacked:
						zone : type_line_zone := get_zone (seg, point_of_attack);
					begin
						log (text => "attack at " & to_string (zone), level => log_threshold + 1);
						
						case zone is
							when START_POINT =>
								set_A_moving (seg);
								object_original_position := get_A (seg);
									
							when END_POINT =>
								set_B_moving (seg);
								object_original_position := get_B (seg);

							when CENTER =>
								set_A_moving (seg);
								set_B_moving (seg);
								object_original_position := point_of_attack;
						end case;
					end query_segment_force;



					-- This procedure queries a segment and sets
					-- its A or B or both ends to "moving" if the caller of the
					-- main procedure has requested a "movable test"
					-- and if the segmet is NOT connected with an port.
					-- Use this procedure in case the caller of the main procedure
					-- requests a "movable test":
					procedure query_segment_movable (seg : in out type_net_segment) is
						use pac_objects;
						
						-- Find the zone at which the segment
						-- is being attacked:
						zone : type_line_zone := get_zone (seg, point_of_attack);
					begin
						log (text => "attack at " & to_string (zone), level => log_threshold + 1);
						log_indentation_up;

						-- By default granting the drag operation is not allowed.
						-- Depending on the attacked zone and the "movable-test"
						-- this flag is set so that the caller gets feedback whether
						-- dragging is allowed or not:
						granted := false;
						
						case zone is
							when START_POINT =>
								if is_movable (A) then
									-- put_line ("set A moving");
									set_A_moving (seg);
									object_original_position := get_A (seg);
									granted := true;
								end if;
									
							when END_POINT =>
								if is_movable (B) then
									-- put_line ("set B moving");
									set_B_moving (seg);
									object_original_position := get_B (seg);
									granted := true;
								end if;

							when CENTER =>
								-- If the segment is attacked at its center,
								-- then both ends must be movable:
								if is_movable (A) and is_movable (B) then
									-- put_line ("set A and B moving");
									set_A_moving (seg);
									set_B_moving (seg);									
									object_original_position := point_of_attack;
									granted := true;
								end if;
						end case;

						log_indentation_down;
					end query_segment_movable;

					
				begin
					if movable_test then -- caller requests a "movable test"
						strand.segments.update_element (
							get_segment (object_cursor), query_segment_movable'access);
					else
						strand.segments.update_element (
							get_segment (object_cursor), query_segment_force'access);
					end if;
				end query_strand;
				
										 
			begin				
				net.strands.update_element (get_strand (object_cursor), query_strand'access);
			end query_net;
			
			
		begin
			module.nets.update_element (get_net (object_cursor), query_net'access);
		end query_module;


		
	begin
		log (text => "module " & to_string (module_cursor)
			& " set A/B of primary segment moving." 
			& " Point of attack " & to_string (point_of_attack),
			level => log_threshold);

		if movable_test then
			log (text => " movable test requested by caller",
				level => log_threshold);
		end if;
			
		log_indentation_up;

		generic_modules.update_element (module_cursor, query_module'access);

		log_indentation_down;
	end set_primary_segment_AB_moving;





	
	

	procedure set_secondary_segments_AB_moving (
		module_cursor	: in pac_generic_modules.cursor;
		object_cursor	: in pac_objects.cursor; -- the primary segment
		log_threshold	: in type_log_level)
	is 
		-- The start and end point of the given primary segment:
		primary_A, primary_B : type_vector_model;
		
		primary_A_moving, primary_B_moving : boolean := false;

		

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is 
				

				procedure query_strand (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;


					-- This procedure queries a secondary segment and
					-- tests whether it is connected with the primary segment:
					procedure query_segment (sec : in out type_net_segment) is begin
						-- Test the connection with the moving primary A end:
						if primary_A_moving then
							if get_A (sec) = primary_A then
								set_A_moving (sec);
							end if;

							if get_B (sec) = primary_A then
								set_B_moving (sec);
							end if;
						end if;
							
						-- Test the connection with the moving primary B end:
						if primary_B_moving then
							if get_A (sec) = primary_B then
								set_A_moving (sec);
							end if;

							if get_B (sec) = primary_B then
								set_B_moving (sec);
							end if;
						end if;
					end query_segment;

					
				begin
					-- Iterate through the segments of the strand
					-- and skip the given primary segment, because we are
					-- interested in secondary segments only:
					while has_element (segment_cursor) loop
						if segment_cursor /= get_segment (object_cursor) then -- skip primary segment
							strand.segments.update_element (segment_cursor, query_segment'access);
						end if;
						
						next (segment_cursor);							
					end loop;
				end query_strand;
				
										 
			begin				
				net.strands.update_element (get_strand (object_cursor), query_strand'access);
			end query_net;
			
			
		begin
			module.nets.update_element (get_net (object_cursor), query_net'access);
		end query_module;
		

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " set A/B of secondary segments moving.",
			level => log_threshold);

		log_indentation_up;
		
		log (text => "Primary segment: " & to_string (object_cursor),
			 level => log_threshold + 1);

		primary_A := get_A (get_segment (object_cursor));
		primary_B := get_B (get_segment (object_cursor));

		primary_A_moving := is_A_moving (get_segment (object_cursor));
		primary_B_moving := is_B_moving (get_segment (object_cursor));
		
		generic_modules.update_element (module_cursor, query_module'access);
		
		log_indentation_down;
	end set_secondary_segments_AB_moving;


	



	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " moving object " 
			-- CS & to_string (object)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_SEGMENT =>
				null; -- CS

			when CAT_STRAND =>
				null; -- CS
				
			when CAT_NET => 
				null; -- CS

			when CAT_LABEL => 
				null; -- CS

			when CAT_LABEL_TAG => 
				null; -- CS
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end move_object;
	



	

	procedure drag_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		point_of_attack	: in type_vector_model;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " dragging object",
			-- CS & to_string (object)
			-- & " to " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_SEGMENT =>
				
				drag_segment (
					module_cursor	=> module_cursor,
					primary_segment	=> object.segment,
					POA				=> point_of_attack,
					destination		=> destination,
					log_threshold	=> log_threshold + 1);

			when CAT_STRAND =>
				null; -- A strand can not be dragged.

			when CAT_NET => 
				null; -- A whole net can not be dragged.

			when CAT_LABEL => 
				null; -- CS

			when CAT_LABEL_TAG => 
				null; -- CS
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end drag_object;







	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " deleting object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_SEGMENT =>
				delete_segment (module_cursor, object.segment, log_threshold + 1);
				
			when CAT_STRAND =>
				delete_strand (module_cursor, object.strand, log_threshold + 1);
				
			when CAT_NET => 
				delete_net (
					module_cursor	=> module_cursor,
					net				=> object.net,
					sheet			=> active_sheet,
					all_sheets		=> modify_net_on_all_sheets,
					log_threshold	=> log_threshold + 1);

				
			when CAT_LABEL => 
				null; -- CS
				
			when CAT_LABEL_TAG => 
				null; -- CS
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end delete_object;


	


	procedure show_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " showing object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_SEGMENT =>
				null;
				-- CS

			when CAT_STRAND =>
				null; -- CS

			when CAT_NET => 
				show_net (module_cursor, object.net.net_cursor, log_threshold + 1);

			when CAT_LABEL => 
				show_label_simple (module_cursor, object.label, log_threshold + 1);

			when CAT_LABEL_TAG => 
				show_label_tag (module_cursor, object.label_tag, log_threshold + 1);
				
			when CAT_VOID =>
				null;
		end case;		
		
		log_indentation_down;
	end show_object;


	
end et_schematic_ops.nets;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
