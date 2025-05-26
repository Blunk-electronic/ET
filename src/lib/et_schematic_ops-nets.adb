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
	



	
	function to_string (
		object	: in type_object_segment)
		return string
	is begin
		return get_net_name (object.net_cursor) 
			& " strand " & get_position (object.strand_cursor)
			& " " & to_string (object.segment_cursor);
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




	function to_string (
		object	: in type_object_strand)
		return string
	is begin
		return get_net_name (object.net_cursor) 
			& " strand " & get_position (object.strand_cursor);
	end to_string;

	


	function to_string (
		object	: in type_object_net)
		return string
	is begin
		return get_net_name (object.net_cursor);
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
				merge_strands (net, strands_on_sheet);
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
				merge_strand (net, strand_temp);
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
		-- See comment in procedure locate_strand.
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
			& " deleting net " & enclose_in_quotes (to_string (net_name)),
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
			& " showing/highlighting whole net "
			& get_net_name (net_cursor),
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end show_net;

	
	


	
	
	procedure delete_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		place			: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level) 
	is
		net_cursor : pac_nets.cursor; -- points to the net


		procedure no_segment is begin
			log (WARNING, "segment not found at" & to_string (position => place) &
			 ". Check net name and position !");
		end;

		
		procedure query_net (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			
			-- Searches the strands of the net for a segment that sits on given place.
			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;
				segment_found, strand_found : boolean := false;


				procedure query_segments (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;
				begin
					while segment_cursor /= pac_net_segments.no_element loop

						-- If segment crosses the given x/y position (in place),
						-- delete the segment.
						if between_A_and_B (
							catch_zone	=> set_catch_zone (center => place.place, radius => 0.0),
							segment		=> segment_cursor) 
						then
							delete (strand.segments, segment_cursor);

							-- signal the calling unit to abort the search
							segment_found := true;

							-- no further search required
							exit;
						end if;

						next (segment_cursor);
					end loop;

					if not segment_found then
						no_segment;
					end if;
					
				end query_segments;

				
			begin -- query_strands
				
				-- Look at strands that are on the given sheet. This loop ends prematurely
				-- as soon as a segment has been found.
				while not segment_found and strand_cursor /= pac_strands.no_element loop
					
					if get_sheet (element (strand_cursor).position) = get_sheet (place) then

						-- signal the calling unit that a strand has been found:
						strand_found := true;

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);

						-- In case no more segments are left in the strand,
						-- remove the now useless strand entirely.
						if is_empty (element (strand_cursor).segments) then
							delete (net.strands, strand_cursor);
						end if;
						
 					end if;
					next (strand_cursor);
				end loop;

				-- Issue warning if no strand has been found.
				if not strand_found then
					no_segment;
				end if;
				
			end query_strands;

			
		begin -- query_net

			-- query the affected strands
			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> query_strands'access);

			-- If the net has no strands anymore, delete it entirely because a
			-- net without strands is useless.
			if is_empty (element (net_cursor).strands) then
				delete (module.nets, net_cursor);
			end if;
			
		end query_net;

		
	begin -- delete_segment
		log (text => "module " & to_string (module_cursor) 
			& " deleting in net " & enclose_in_quotes (to_string (net_name))
			& " segment at" & enclose_in_quotes (to_string (position => place)),
			level => log_threshold);

		-- locate the requested nets in the module
		net_cursor := locate_net (module_cursor, net_name);

		-- issue error if net does not exist:
		if net_cursor = pac_nets.no_element then
			net_not_found (net_name);
		end if;

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_net'access);

		update_ratsnest (module_cursor, log_threshold + 1);
		
		log_indentation_down;		
	end delete_segment;




	

	
	function is_movable (
		module_cursor	: in pac_generic_modules.cursor;
		segment			: in type_net_segment;
		zone			: in type_line_zone;
		point_of_attack	: in type_object_position;
		log_threshold	: in type_log_level) 
		return boolean 
	is
		result : boolean := true; -- to be returned. true means the zone is movable.
		-- Goes false once a port has been found in the given zone.

		point : type_object_position;

		procedure search_ports is
		-- Searches ports of devices, netchangers and submodules that sit on
		-- the point of interest.	
		-- On the first finding, sets result to false and finishes. If no 
		-- finding, result remains true.	
			use pac_device_ports;
			use pac_submodule_ports;

			use et_netlists;
			use pac_netchanger_ports;

			device : pac_device_ports.cursor := segment.ports.devices.first;
			submodule : pac_submodule_ports.cursor := segment.ports.submodules.first;
			netchanger : pac_netchanger_ports.cursor := segment.ports.netchangers.first;

			use et_schematic_ops.submodules;

			
		begin -- search_ports
			while device /= pac_device_ports.no_element loop

				if get_position (
					module_cursor	=> module_cursor,
					device_name		=> element (device).device_name,
					port_name		=> element (device).port_name,
					log_threshold	=> log_threshold + 2) 
					
					= point then

					result := false; -- not movable
					exit;

				end if;
				
				next (device);
			end loop;

			-- if no device port found, search in submodule ports
			if result = true then

				while submodule /= pac_submodule_ports.no_element loop

					if get_submodule_port_position ( -- CS use a similar function that takes only cursors ?
						module_name		=> key (module_cursor),
						submod_name		=> element (submodule).module_name,
						port_name		=> element (submodule).port_name,
						log_threshold	=> log_threshold + 2) 
						
						= point then

						result := false; -- not movable
						exit;

					end if;
					
					next (submodule);
				end loop;

			end if;

			-- if no submodule port found, search in netchanger ports
			if result = true then

				while netchanger /= pac_netchanger_ports.no_element loop

					if get_netchanger_port_position ( -- CS use a similar function that takes only cursors ?
						module_name		=> key (module_cursor),
						index			=> element (netchanger).index,
						port			=> element (netchanger).port,
						log_threshold	=> log_threshold + 2) 
						
						= point then

						result := false; -- not movable
						exit;

					end if;
					
					next (netchanger);
				end loop;

			end if;

			-- if no port found, result is still true
		end search_ports;

		
	begin
		log_indentation_up;
		
		-- The point of interest is on the sheet specified in argument "point_of_attack".
		-- The x/y coordinates are taken from the segment start or end point.
		
		case zone is
			when START_POINT =>
				point := to_position (
						point => get_A (segment),
						sheet => get_sheet (point_of_attack));

				search_ports; -- sets result to false if a port is connected with the start point
				
			when END_POINT =>
				point := to_position (
						point => get_B (segment),
						sheet => get_sheet (point_of_attack));

				search_ports; -- sets result to false if a port is connected with the end point
				
			when CENTER =>
				-- Both start and end point must be checked for any ports.
				-- First check the start point of the segment.
				-- If start point is movable, then the end point must be checked too.
				point := to_position (
						point => get_A (segment),
						sheet => get_sheet (point_of_attack));

				search_ports; -- sets result to false if a port is connected with the start point

				-- If start point is movable, check end point.
				if result = true then
					point := to_position (
						point => get_B (segment),
						sheet => get_sheet (point_of_attack));

					search_ports; -- sets result to false if a port is connected with the end point
				end if;
		end case;

		log_indentation_down;
		
		return result;
	end is_movable;





	
	
	
	procedure drag_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		point_of_attack	: in type_object_position; -- sheet/x/y
		coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model; -- x/y, the new position 
		log_threshold	: in type_log_level) 
	is
		net_cursor : pac_nets.cursor; -- points to the net


		procedure no_segment is begin
			log (WARNING, "No segment found at " & to_string (position => point_of_attack) &
			 ". Check net name and position !");
		end;
		
		procedure query_net (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			
			-- Searches the strands of the net for a segment that sits on given point_of_attack.
			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;
				segment_found, strand_found : boolean := false;

				
				procedure query_segments (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;
					segment_cursor_target : pac_net_segments.cursor;
					target_segment_before : type_net_segment;

					zone : type_line_zone;

					
					procedure move_targeted_segment (segment : in out type_net_segment) is 
						-- backup the segment as it was before the move/drag:
						segment_before : constant type_net_segment := segment;
					begin
						case coordinates is
							when ABSOLUTE =>
								--log (text => "move targeted segment absolute", level => log_threshold + 3);
								attack (segment, point_of_attack.place, destination);

							when RELATIVE =>
								--log (text => "move targeted segment relative", level => log_threshold + 3);
								case zone is
									when START_POINT =>
										move_start_by (segment, destination);

									when END_POINT =>
										move_end_by (segment, destination);
										
									when CENTER =>
										move_start_by (segment, destination);
										move_end_by (segment, destination);
								end case;										
						end case;

						-- CS
						-- move_net_labels (
						-- 	segment_before	=> segment_before,
						-- 	segment_after	=> segment,
						-- 	zone			=> zone);
						
					end move_targeted_segment;

					
					-- This procedure moves the start/end points of segments that are connected
					-- with the target_segment_before.
					procedure move_connected_segment (connected_segment : in out type_net_segment) is 

						-- backup the segment as it was before the move/drag:
						segment_before : constant type_net_segment := connected_segment;

						
						procedure copy_A is begin
							if get_A (connected_segment) = get_A (target_segment_before) then
								
								-- The connected segment is being dragged at its start point:
								set_A (connected_segment, get_A (segment_cursor_target));

								-- CS
								-- move_net_labels (
								-- 	segment_before	=> segment_before,
								-- 	segment_after	=> connected_segment,
								-- 	zone			=> START_POINT);

							end if;

							if get_B (connected_segment)= get_A (target_segment_before) then
								
								-- The connected segment is being dragged at its end point:
								set_B (connected_segment, get_A (segment_cursor_target));

								-- CS
								-- move_net_labels (
								-- 	segment_before	=> segment_before,
								-- 	segment_after	=> connected_segment,
								-- 	zone			=> END_POINT);

							end if;
						end;

						
						procedure copy_B is begin
							if get_A (connected_segment) = get_B (target_segment_before) then

								-- The connected segment is being dragged at its start point:
								set_A (connected_segment, get_B (segment_cursor_target));

								-- CS
								-- move_net_labels (
								-- 	segment_before	=> segment_before,
								-- 	segment_after	=> connected_segment,
								-- 	zone			=> START_POINT);

							end if;

							if get_B (connected_segment) = get_B (target_segment_before) then
								-- The connected segment is being dragged at its end point:
								
								set_B (connected_segment, get_B (segment_cursor_target));

								-- CS
								-- move_net_labels (
								-- 	segment_before	=> segment_before,
								-- 	segment_after	=> connected_segment,
								-- 	zone			=> END_POINT);

							end if;
						end;

						
					begin -- move_connected_segment
						case zone is
							when START_POINT => 
								-- The segment start or end point moves to the targeted segment start point.
								copy_A; 
								
							when END_POINT => 
								-- The segment start or end point moves to the targeted segment end point.
								copy_B;
								
							when CENTER => 
								-- The segment start or end point moves to the targeted segment start point.
								copy_A; 

								-- The segment start or end point moves to the targeted segment end point.
								copy_B;
						end case;
					end move_connected_segment;

					
					procedure connect_ports (segment : in out type_net_segment) is
					-- Looks up ports of devices, netchangers or submodules that are 
					-- to be connected with the segment. The place where ports are
					-- searched depends on the zone that has been moved.
					-- (The given segment sits already at the new position.)
						ports : type_ports;

						procedure append_portlists is 
						-- Append the portlists obtained from function ports_at_place
						-- to the segment.
						-- CS: Special threatment required if a port is among the portlists
						-- that is already somewhere in the strand. 
						-- This particular port must be exempted from the appending.
						-- Currently only the integrity check (procedure check_integrity)
						-- detects this rare case.
						begin
							pac_device_ports.union (segment.ports.devices, ports.devices);
							pac_submodule_ports.union (segment.ports.submodules, ports.submodules);
							et_netlists.pac_netchanger_ports.union (segment.ports.netchangers, ports.netchangers);
						end append_portlists;

						
					begin -- connect_ports
						case zone is
							when START_POINT =>
								ports := ports_at_place 
									(
									module_cursor	=> module_cursor, 
									place 			=> to_position (
													point => get_A (segment),
													sheet => get_sheet (point_of_attack)),
									log_threshold => log_threshold + 1
									);

								append_portlists;

								
							when END_POINT =>
								ports := ports_at_place 
									(
									module_cursor	=> module_cursor, 
									place 			=> to_position (
													point => get_B (segment),
													sheet => get_sheet (point_of_attack)),
									log_threshold => log_threshold + 1
									);

								append_portlists;

								
							when CENTER =>
								ports := ports_at_place 
									(
									module_cursor	=> module_cursor, 
									place 			=> to_position (
													point => get_A (segment),
													sheet => get_sheet (point_of_attack)),
									log_threshold => log_threshold + 1
									);

								append_portlists;
								
								ports := ports_at_place 
									(
									module_cursor	=> module_cursor, 
									place 			=> to_position (
													point => get_B (segment),
													sheet => get_sheet (point_of_attack)),
									log_threshold => log_threshold + 1
									);
								
								append_portlists;
						end case;
					end connect_ports;						

					
				begin -- query_segments
					-- MOVE TARGETED SEGMENT
					while segment_cursor /= pac_net_segments.no_element loop

						-- If segment crosses the given x/y position (in point_of_attack) then
						-- the segment has been found:
						if on_segment (
							catch_zone	=> set_catch_zone (point_of_attack.place, accuracy_default),
							segment		=> segment_cursor)
						then
							--log (text => "point of attack sits on segment", level => log_threshold + 1);
							
							-- Calculate the zone of attack:
							zone := get_zone (
								point	=> point_of_attack.place,
								line	=> element (segment_cursor));

							-- depending on zone, drag start point, end point or both
							log (text => "dragging " & to_string (element (segment_cursor))
								& " at " & type_line_zone'image (zone), level => log_threshold + 1);

							-- Test whether the zone is movable. If not movable, nothing happens.
							if is_movable (
								module_cursor, element (segment_cursor),
								zone, point_of_attack, log_threshold + 1)
							then
								
								-- Backup the cursor of the targeted segment.
								-- Backup the segment as it was BEFORE the dragging.
								-- They are required later.
								segment_cursor_target := segment_cursor;
								target_segment_before := element (segment_cursor);

								-- move the targeted segment
								pac_net_segments.update_element (
									container	=> strand.segments,
									position	=> segment_cursor,
									process		=> move_targeted_segment'access);

								-- Signal the caller to abort the search as a suitable
								-- segment has been found now:
								segment_found := true;

								-- no further search required
								exit;

							else
								log (WARNING, "Segment is tied to a port. Dragging not possible !");
							end if;

						end if;

						next (segment_cursor);
					end loop;

					
					if segment_found then

						-- MOVE SEGMENTS CONNECTED WITH THE TARGETED SEGMENT. 
						-- Iterate in segments. skip targeted segment because it has been dragged
						-- already (see above).
						segment_cursor := strand.segments.first; -- reset segment cursor
						while segment_cursor /= pac_net_segments.no_element loop
							if segment_cursor /= segment_cursor_target then

								pac_net_segments.update_element (
									container	=> strand.segments,
									position	=> segment_cursor,
									process		=> move_connected_segment'access);

							end if;

							next (segment_cursor);
						end loop;

						-- update strand position
						set_strand_position (strand);

						-- Look for ports at the start/end points of the segment. The segment
						-- is now at the new position (either start point or end point or both).
						-- If any port (of a device, netchanger or submodule) sits there, it must be
						-- connected with the segment. That means adding these ports to the segment.
						update_element (
							container	=> strand.segments,
							position	=> segment_cursor_target,
							process		=> connect_ports'access);

					end if;
				end query_segments;

				
			begin -- query_strands
				
				-- Look at strands that are on the given sheet. This loop ends prematurely
				-- as soon as a segment has been found.
				while not segment_found and strand_cursor /= pac_strands.no_element loop
					
					if get_sheet (element (strand_cursor).position) = get_sheet (point_of_attack) then
						log (text => "searching strand at" 
							 & to_string (element (strand_cursor).position),
							level => log_threshold + 1);
						
						-- signal the calling unit that a strand has been found:
						strand_found := true;

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);

					end if;
					
					next (strand_cursor);
				end loop;

				-- Issue warning if no strand or no net segment has been found.
				if not strand_found or not segment_found then
					no_segment;
				end if;
				
			end query_strands;

			
		begin

			-- query the affected strands
			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> query_strands'access);
			
		end query_net;


		praeamble : constant string := "module " & to_string (module_cursor)
			& " dragging segment of net " & enclose_in_quotes (to_string (net_name))
			& " / point of attack" & to_string (position => point_of_attack)
			& " ";
		
	begin -- drag_segment
		case coordinates is
			when ABSOLUTE =>
				log (text => praeamble & " to" & to_string (destination),
					level => log_threshold);

			when RELATIVE =>
				log (text => praeamble & " by" & to_string (destination),
					level => log_threshold);

		end case;
		

		-- locate the requested nets in the module
		net_cursor := locate_net (module_cursor, net_name);

		-- issue error if net does not exist:
		if net_cursor = pac_nets.no_element then
			net_not_found (net_name);
		end if;

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_net'access);

		update_ratsnest (module_cursor, log_threshold + 1);
		
		log_indentation_down;		
	end drag_segment;





	procedure drag_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in pac_nets.cursor;
		strand_cursor	: in pac_strands.cursor;
		segment_cursor	: in pac_net_segments.cursor;
		point_of_attack	: in type_vector_model;
		coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_vector_model; -- x/y
		log_threshold	: in type_log_level)
	is

	begin
		-- CS
		null;
	end drag_segment;



	
	

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




	
	
	procedure insert_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in out pac_nets.cursor;
		sheet			: in type_sheet;
		net_name		: in pac_net_name.bounded_string;
		segment_new		: in type_net_segment;
		log_threshold	: in type_log_level)
	is 
		-- The segment being processed.
		-- Initially it is just a copy of the given bare segment. 
		-- In the course of this procedure ports of devices, submodules 
		-- and netchangers will be attached to it:
		segment : type_net_segment := segment_new;
		
		point : type_object_position;

		type type_junction is record
			required	: boolean := false;
			place		: type_object_position;
		end record;

		junction_at_A : type_junction;
		junction_at_B	: type_junction;
		
		use pac_net_names;
		net_names : pac_net_names.list;

		
		-- Returns the content of net_names in a single string.
		function list_nets return string is 
			net_cursor : pac_net_names.cursor := net_names.first;
			use ada.strings.unbounded;
			names : ada.strings.unbounded.unbounded_string;
		begin
			while net_cursor /= pac_net_names.no_element loop
				names := names & to_string (element (net_cursor)) & space;
				next (net_cursor);
			end loop;
			return to_string (names);
		end;

		
		procedure collision (point : in type_object_position) is begin
			raise semantic_error_1 with
				"ERROR: Net segment collides at" & to_string (position => point) 
				& " with net(s): " & list_nets & " !";
		end collision;
		
		ports : type_ports;

		
		-- Attaches the ports to the segment being processed:
		procedure assign_ports_to_segment is begin
			pac_device_ports.union (segment.ports.devices, ports.devices);
			pac_submodule_ports.union (segment.ports.submodules, ports.submodules);
			et_netlists.pac_netchanger_ports.union (segment.ports.netchangers, ports.netchangers);
		end;

		
		procedure create_net (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			inserted : boolean;
			strand : type_strand;
			net : type_net;

			-- Issues error message and raises constraint_error if net_names contains
			-- any foreign net names.
			procedure evaluate_net_names (point : in type_object_position) is 
			begin
				if not is_empty (net_names) then
					collision (point);
				end if;
			end;
			
		begin -- create_net
		
			------------
			-- Test whether any foreign nets cross the start point of the segment:
			point := to_position (
					sheet => sheet,
					point => get_A (segment_new));
			
			net_names := get_nets_at_place (
					module_cursor	=> module_cursor,
					place			=> point,
					log_threshold	=> log_threshold);

			evaluate_net_names (point);
			
			-- Test whether any foreign nets cross the end point of the segment:
			point := to_position (
					sheet => sheet,
					point => get_B (segment_new));
			
			net_names := get_nets_at_place (
					module_cursor	=> module_cursor,
					place			=> point,
					log_threshold	=> log_threshold);

			evaluate_net_names (point);
			-------------
			


			
			-----------
			-- look for any ports at start point of the new net segment
			ports := ports_at_place (
					module_cursor	=> module_cursor, 
					place			=> to_position (
										sheet => sheet,
										point => get_A (segment_new)),
					log_threshold	=> log_threshold);

			assign_ports_to_segment;

			-- look for any ports at end point of the new net segment
			-- The end point is just x/y. The sheet must be derived from the start point.
			ports := ports_at_place (
					module_cursor	=> module_cursor, 
					place			=> to_position (
										sheet => sheet,
										point => get_B (segment_new)),
					log_threshold	=> log_threshold);

			assign_ports_to_segment;
			------------


			
			-- insert segment in strand
			pac_net_segments.append (
				container	=> strand.segments,
				new_item	=> segment);

			-- set the sheet number of the strand
			set_sheet (strand.position, sheet);
			
			-- set lowest x/y position of strand
			set_strand_position (strand);

			-- insert the strand in the net
			pac_strands.append (
				container	=> net.strands,
				new_item	=> strand);
			
			-- insert the net in the module
			insert (
				container	=> module.nets,
				key			=> net_name,
				inserted	=> inserted,
				new_item	=> net,
				position	=> net_cursor);
						
		end create_net;

		
		procedure extend_net (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			attach_to_strand : boolean := false;

			-- Issues error message and raises constraint_error if net_names contains
			-- any net names but the given net_name.
			-- If net_names contains the given net_name, then the flag attach_to_strand
			-- is set. The strand will be extended later by the segment specified by 
			-- A and B.
			procedure evaluate_net_names (point : in type_object_position) is 
			begin
				if is_empty (net_names) then -- no nets here
					null;
					
				else -- there are nets
					if contains (net_names, net_name) then
						-- segment will be attached to an already existing strand
						attach_to_strand := true; 
						
					else
						-- Segment collides with foreign nets.
						collision (point);
					end if;
				end if;
			end evaluate_net_names;

			
			type type_which_strand is record
				cursor				: pac_strands.cursor;
				junction_required	: boolean := false;
			end record;

			strand_at_start : type_which_strand;
			strand_at_end   : type_which_strand;

			
			-- Required to test whether the new segment will be a dead end
			-- at its start or end point:
			function dead_end (strand : in type_which_strand) return boolean is begin
				if strand.cursor = pac_strands.no_element then
					return true;
				else 
					return false;
				end if;
			end dead_end;

			
			-- Returns a cursor to the strand at place and a flag whether to place
			-- a junction at the given place.
			function which_strand (place : in type_object_position) 
				return type_which_strand 
			is
				result : type_which_strand; -- to be returned

				-- Searches strands of given net for a segment that crosses place.
				-- Cancels search on first match.
				procedure query_strands (
					net_name	: in pac_net_name.bounded_string;
					net			: in type_net) is
					segment_found : boolean := false;
					
					-- Iterate segments until first match.
					procedure query_segments (strand : in type_strand) is
						segment_cursor : pac_net_segments.cursor := strand.segments.first;
					begin
						while segment_cursor /= pac_net_segments.no_element loop

							-- Test if place sits on segment.
							if on_line (
								point 	=> place.place,
								line	=> element (segment_cursor)) then

-- 								-- It is not allowed to place a junction in a sloped segment,
-- 								-- because splitting sloping segments seems a rare, difficult and dangerous task.
-- 								if segment_orientation (segment_cursor) = SLOPING then
-- 									junction_in_sloping_segment (place);
-- 								end if;

-- 								-- signal "strand iterator" to abort search prematurely
-- 								segment_found := true;
								
								-- test whether a junction is required at place
								if between_A_and_B (set_catch_zone (place.place, 0.0), segment_cursor) then

									-- It is not allowed to place a junction in a sloped segment,
									-- because splitting sloping segments seems a rare, difficult and dangerous task.
									if get_segment_orientation (segment_cursor) = SLOPING then
										junction_in_sloping_segment (place);
									end if;
									
									result.junction_required := true;
								end if;

								-- signal "strand iterator" to abort search prematurely
								segment_found := true;
								
								exit; -- no further search required. 
								
								-- segment_cursor points to the segment just found
							end if;
							
							next (segment_cursor);
						end loop;
					end query_segments;

					
				begin -- query_strands
					result.cursor := net.strands.first;

					-- Iterate strands. Cancel prematurely once a segment has been found.
					-- Look at strands on the relevant sheet only.
					while result.cursor /= pac_strands.no_element loop
						if get_sheet (element (result.cursor).position) = get_sheet (place) then

							pac_strands.query_element (
								position	=> result.cursor,
								process		=> query_segments'access);

							if segment_found then exit; end if;
							
						end if;
						next (result.cursor);
					end loop;
				end query_strands;

				
			begin -- which_strand
				query_element (
					position	=> net_cursor,
					process		=> query_strands'access);
				
				return result;
			end which_strand;

			
			procedure append_segment (strand : in out type_strand) is begin
				pac_net_segments.append (strand.segments, segment);
				set_strand_position (strand);
			end append_segment;

			
			-- Locates the strand (indicated by strand_at_start)
			-- and appends the new segment to it. 
			procedure extend_strand_start (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is begin
				pac_strands.update_element (
					container	=> net.strands,
					position	=> strand_at_start.cursor,
					process		=> append_segment'access);
			end extend_strand_start;

			
			-- Locates the strand (indicated by strand_at_end)
			-- and appends the new segment to it. 
			procedure extend_strand_end (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is begin
				pac_strands.update_element (
					container	=> net.strands,
					position	=> strand_at_end.cursor,
					process		=> append_segment'access);
			end extend_strand_end;

			
			-- Creates a new strand that contains the segment.
			procedure create_strand (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand : type_strand; -- the new strand
			begin				
				-- look for any ports at start point of the new net segment
				ports := ports_at_place (
						module_cursor	=> module_cursor, 
						place			=> to_position (
											sheet	=> sheet,
											point	=> get_A (segment_new)),
						log_threshold	=> log_threshold + 2);

				assign_ports_to_segment;

				-- look for any ports at end point of the new net segment
				ports := ports_at_place (
						module_cursor	=> module_cursor, 
						place			=> to_position (
											sheet => sheet,
											point => get_B (segment_new)),
						log_threshold	=> log_threshold + 2);

				assign_ports_to_segment;
				

				-- insert the given segment in the new strand
				pac_net_segments.append (
					container	=> strand.segments,
					new_item	=> segment);

				-- set the sheet number of the strand
				set_sheet (strand.position, sheet);
				
				-- set lowest x/y position of strand
				set_strand_position (strand);

				-- insert the strand in the net
				pac_strands.append (
					container	=> net.strands,
					new_item	=> strand);

			end create_strand;

			
			-- Merges two strands indicated by strand_at_start and strand_at_end.
			-- The strand_at_start will merge into strand_at_end.
			-- strand_at_start will be gone in the end. All its segments will move to 
			-- strand_at_end.
			procedure merge_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				-- Get the segments of the strand that will be removed. These segments will
				-- move into the final strand.
				segments_source : pac_net_segments.list := element (strand_at_start.cursor).segments;
				
				procedure merge (strand : in out type_strand) is begin
				-- Appends to the segments of strand at start point
				-- the segments_source.
					pac_net_segments.splice (
						target	=> strand.segments,
						before	=> pac_net_segments.no_element, -- default, means just appending after target
						source	=> segments_source);

					-- update strand position
					set_strand_position (strand);
				end merge;
					
			begin -- merge_strands
				log (text => "merging strands ...", level => log_threshold + 2);
				
				-- Append segments_source to the strand indicated by strand_at_end:
				pac_strands.update_element (
					container	=> net.strands,
					position	=> strand_at_end.cursor,
					process		=> merge'access);

				-- Delete the "source" strand. Its segments are already part of strand_at_end.
				pac_strands.delete (
					container	=> net.strands,
					position	=> strand_at_start.cursor);
				
			end merge_strands;	

			-- Returns true when strand at start and strand at end are equal.
			-- This is the case when the operator tries to draw multiple/redundant
			-- connections in a strand:
			function redundant_connection return boolean is begin

				-- 1st condition: Both ends of the new segment must connect
				-- with a strand.
				if strand_at_start.cursor /= pac_strands.no_element and
					strand_at_end.cursor /= pac_strands.no_element then

					-- 2nd condition: Both ends of the new segment must connect
					-- with the same strand:
					if element (strand_at_start.cursor) = element (strand_at_end.cursor) then
						return true;
					else
						return false;
					end if;

				else
					return false;
				end if;
			end redundant_connection;
			
			
		begin -- extend_net
			------------
			-- Obtain the names of nets that cross the START point of the segment:
			point := to_position (
					sheet => sheet,
					point => get_A (segment_new));

			net_names := get_nets_at_place (
					module_cursor	=> module_cursor,
					place			=> point,
					log_threshold	=> log_threshold);

			evaluate_net_names (point); -- modifies the attach_to_strand flag
			
			-- Obtain the names of nets that cross the END point of the segment:
			point := to_position (
					sheet => sheet,
					point => get_B (segment_new));
			
			net_names := get_nets_at_place (
					module_cursor	=> module_cursor,
					place			=> point,
					log_threshold	=> log_threshold);

			evaluate_net_names (point); -- modifies the attach_to_strand flag
			-------------

			-- 1. Now we know the segment_new is acceptable and valid. Means the start
			--    and end points do not collide with foreign nets.
			-- 2. We also know whether to attach the segment to an existing strand
			--    or whether the segment is going to start a new strand.
			
			if attach_to_strand then
				log (text => "attaching segment to strand ...", level => log_threshold + 1);
				log_indentation_up;

				-- The START point of the new segment could join a strand.				
				-- Obtain the cursor to the strand that crosses the START point:
				strand_at_start := which_strand (to_position (
									sheet	=> sheet,
									point	=> get_A (segment_new)));

				-- The END point of the new segment could join a strand.
				-- Obtain the cursor to the strand that crosses the END point:
				strand_at_end := which_strand (to_position (
									sheet => sheet,
									point => get_B (segment_new)));

				-- The new segment must not be a redundant connection inside a strand:
				if not redundant_connection then
				
					-- Determine whether a junction will be placed here later:
					if not dead_end (strand_at_start) then
						-- The start point will be connected with a strand:
						log (text => "with its start point at " & 
							to_string (position => to_position (
														sheet	=> sheet,
														point	=> get_A (segment_new))),
							level => log_threshold + 2);

						-- If required, prepare placing a junction at start point of segment.
						-- The junction will be placed later.
						if strand_at_start.junction_required then
							junction_at_A.required := true;
							junction_at_A.place := to_position (
														sheet	=> sheet,
														point	=> get_A (segment_new));
						end if;
					end if;

					-- collect ports at dead end or where a junction is to be placed:
					if dead_end (strand_at_start) or junction_at_A.required then
						
						-- look for any ports at start point of the new segment
						ports := ports_at_place (
								module_cursor	=> module_cursor, 
								place			=> to_position (
													sheet	=> sheet,
													point	=> get_A (segment_new)),
								log_threshold	=> log_threshold + 2);

						assign_ports_to_segment;
					end if;
					----------
					

					-- Determine whether a junction will be placed here later:
					if not dead_end (strand_at_end) then
						-- The end point will be connected with a strand:
						log (text => "with its end point at " & to_string (
									position => to_position (
										sheet => sheet,
										point => get_B (segment_new))
										),
							level => log_threshold + 2);

						-- If required, prepare placing a junction at end point of segment.
						-- The junction will be placed later.
						if strand_at_end.junction_required then
							junction_at_B.required := true;
							junction_at_B.place := to_position (
										sheet => sheet,
										point => get_B (segment_new));
						end if;
					end if;
					
					-- collect ports at dead end or where a junction is to be placed:
					if dead_end (strand_at_end) or junction_at_B.required then

						-- look for any ports at end point of the new segment
						ports := ports_at_place (
								module_cursor	=> module_cursor, 
								place			=> to_position (
													sheet => sheet,
													point => get_B (segment_new)),
								log_threshold	=> log_threshold + 2);

						assign_ports_to_segment;
					end if;

					-- If segment_new is to extend a strand
					-- then the strands at start or/and end point must be extended
					-- by segment_new.
					if not dead_end (strand_at_start) xor not dead_end (strand_at_end) then
						if not dead_end (strand_at_start) then
							pac_nets.update_element (
								container	=> module.nets,
								position	=> net_cursor,
								process		=> extend_strand_start'access);
						end if;

						if not dead_end (strand_at_end) then
							pac_nets.update_element (
								container	=> module.nets,
								position	=> net_cursor,
								process		=> extend_strand_end'access);
						end if;
					end if;
					-----------

					-- If both ends are to be connected with a strand,
					-- we have to merge both strands:
					if not dead_end (strand_at_start) and not dead_end (strand_at_end) then
						log_indentation_up;

						-- The new segment must first be attached to one of the
						-- two strands.
						pac_nets.update_element (
							container	=> module.nets,
							position	=> net_cursor,
							process		=> extend_strand_start'access);
						
						-- Merge the two strands indicated by 
						-- strand_at_start and strand_at_end:
						pac_nets.update_element (
							container	=> module.nets,
							position	=> net_cursor,
							process		=> merge_strands'access);

						log_indentation_down;
					end if;

				else
					raise semantic_error_1 with
						"ERROR: Attempt to draw redundant connection rejected !";
				end if;
				
				log_indentation_down;
				
			else
				-- A new strand must be created in the net.
				-- The strand will contain the new segment:
				pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> create_strand'access);
				
			end if;
			
		end extend_net;

		
	begin -- insert_segment

		-- If no net named after net_name exists yet, notify operator that a 
		-- new net will be created.
		-- If the net already exists, extend it by the given net segment segment_new.
		if net_cursor = pac_nets.no_element then

			-- net does not exist yet
			log (text => "creating new net " & to_string (net_name), level => log_threshold);

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> create_net'access);
		else
			-- net exists. extend the net by the given net segment
			log (text => "extending net " & to_string (net_name), level => log_threshold);
			log_indentation_up;
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> extend_net'access);

			-- place junctions if required
			if junction_at_A.required then
				place_junction (
					module_cursor	=> module_cursor,
					place			=> junction_at_A.place,
					log_threshold	=> log_threshold + 1);
			end if;

			if junction_at_B.required then
				place_junction (
					module_cursor	=> module_cursor,
					place			=> junction_at_B.place,
					log_threshold	=> log_threshold + 1);
			end if;

			
			log_indentation_down;
		end if;

	end insert_segment;




	
	
	procedure insert_net (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		A				: in type_object_position; -- sheet/x/y
		B				: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is		
		net_cursor : pac_nets.cursor; -- points to the net
		segment : type_net_segment;
	begin
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) &
			" inserting net " & enclose_in_quotes (to_string (net_name)) &
			" segment from" & to_string (position => A) &
			" to" & to_string (B), level => log_threshold);
		
		-- The net can be in the module already. Locate the requested net in the module.
		-- net_cursor will point to no_element if the net is not already there.
		net_cursor := locate_net (module_cursor, net_name);

		-- build the segment from given start and end point
		set_A (segment, A.place);
		set_B (segment, B);
		
		log_indentation_up;

		insert_segment (
			module_cursor, net_cursor, get_sheet (A),
			net_name, segment, log_threshold + 1);

		update_ratsnest (module_cursor, log_threshold + 2);
		
		log_indentation_down;		
	end insert_net;





	

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
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) 
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
					old_segment_orientation : type_net_segment_orientation; -- horizontal, vertical, sloped

					
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
								when HORIZONTAL =>
									iterate (old_segment.labels, query_labels_horizontal'access);

								when VERTICAL =>
									iterate (old_segment.labels, query_labels_vertical'access);

								when SLOPING => raise constraint_error; -- CS should never happen
							end case;
							log_indentation_down;
						end update_labels;

						
						-- Queries the positions of the device ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
						procedure update_device_ports is 
							use pac_device_ports;
							use et_symbols;

							
							procedure query_ports (cursor : in pac_device_ports.cursor) is
								device_name 	: type_device_name; -- IC23
								port_name		: pac_port_name.bounded_string; -- CE
								port_position 	: type_vector_model; -- the xy-position of the port
							begin
								device_name	:= element (cursor).device_name;
								port_name	:= element (cursor).port_name;

								-- locate the port by module, device and port name:
								port_position := get_position (module_cursor, device_name, port_name, log_threshold + 1).place;
								log_indentation_up;
								
								log (text => "device " & to_string (device_name) & " port " & to_string (port_name) &
									" at" & to_string (port_position),
									level => log_threshold + 1);

								-- If the port was at the start point of the old segment, then
								-- it goes into segment_1.
								if port_position = get_A (old_segment) then
									insert (segment_1.ports.devices, element (cursor));

								-- If the port was at the end point of the old segment, then
								-- it goes into segment_2.
								elsif port_position = get_B (old_segment) then
									insert (segment_2.ports.devices, element (cursor));

								-- If port was somewhere else, we have a problem. This should never happen.
								else
									log (ERROR, "port not on segment !");
									raise constraint_error;
								end if;
								
								log_indentation_down;
							end query_ports;
							
							
						begin -- update_device_ports
							log (text => "updating device ports ...", level => log_threshold + 1);
							log_indentation_up;
							
							iterate (old_segment.ports.devices, query_ports'access);
							log_indentation_down;
						end update_device_ports;

						
						-- Queries the positions of the submodule ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
						procedure update_submodule_ports is 
							use pac_submodule_ports;

							
							procedure query_ports (cursor : in pac_submodule_ports.cursor) is
								use et_schematic_ops.submodules;
								submod_name 	: pac_module_instance_name.bounded_string; -- MOT_DRV_3
								port_name		: pac_net_name.bounded_string; -- RESET
								port_position 	: type_vector_model; -- the xy-position of the port
							begin
								submod_name	:= element (cursor).module_name; -- CLOCK_GENERATOR
								port_name	:= element (cursor).port_name;	-- RESET

								-- locate the port by module, submodule and port name:
								port_position := get_submodule_port_position (module_name, submod_name, port_name, log_threshold + 1).place;
								log_indentation_up;
								
								log (text => "submodule " & to_string (submod_name) & " port " & to_string (port_name) &
									" at" & to_string (port_position),
									level => log_threshold + 1);

								-- If the port was at the start point of the old segment, then
								-- it goes into segment_1.
								if port_position = get_A (old_segment) then
									insert (segment_1.ports.submodules, element (cursor));

								-- If the port was at the end point of the old segment, then
								-- it goes into segment_2.
								elsif port_position = get_B (old_segment) then
									insert (segment_2.ports.submodules, element (cursor));

								-- If port was somewhere else, we have a problem. This should never happen.
								else
									log (ERROR, "port not on segment !");
									raise constraint_error;
								end if;
								
								log_indentation_down;
							end query_ports;

							
						begin -- update_submodule_ports
							log (text => "updating submodule ports ...", level => log_threshold + 1);
							log_indentation_up;
							
							iterate (old_segment.ports.submodules, query_ports'access);
							log_indentation_down;
						end update_submodule_ports;

						
						-- Queries the positions of the netchanger ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
						procedure update_netchanger_ports is 
							use et_netlists;
							use et_netlists.pac_netchanger_ports;
							use et_submodules;

							
							procedure query_ports (cursor : in pac_netchanger_ports.cursor) is
								index			: type_netchanger_id; -- 1,2,3,...
								port			: type_netchanger_port_name; -- SLAVE/MASTER
								port_position 	: type_vector_model; -- the xy-position of the port
							begin
								index := element (cursor).index;
								port := element (cursor).port;

								-- locate the port by module, netchanger index and port:
								port_position := et_schematic_ops.submodules.get_netchanger_port_position (
									module_name, index, port, log_threshold + 1).place;
								
								log_indentation_up;
								
								log (text => "netchanger " & to_string (index) & " port " & to_string (port) &
									" at" & to_string (port_position),
									level => log_threshold + 1);

								-- If the port was at the start point of the old segment, then
								-- it goes into segment_1.
								if port_position = get_A (old_segment) then
									insert (segment_1.ports.netchangers, element (cursor));

								-- If the port was at the end point of the old segment, then
								-- it goes into segment_2.
								elsif port_position = get_B (old_segment) then
									insert (segment_2.ports.netchangers, element (cursor));

								-- If port was somewhere else, we have a problem. This should never happen.
								else
									log (ERROR, "port not on segment !");
									raise constraint_error;
								end if;
								
								log_indentation_down;
							end query_ports;

							
						begin -- update_netchanger_ports
							log (text => "updating netchanger ports ...", level => log_threshold + 1);
							log_indentation_up;
							
							iterate (old_segment.ports.netchangers, query_ports'access);
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
							if old_segment_orientation = SLOPING then
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
		return get_net_name (object.net_cursor)
		 & " " & get_position (object.label_cursor);
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
			& " modifying status of net label "
			& get_net_name (label.net_cursor) 
			& " strand " & get_position (label.strand_cursor)
			& " " & to_string (label.segment_cursor)
			& " " & get_position (label.label_cursor)
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
			& " looking up the first net label / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		
		return result;
	end get_first_label;




	
	
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
						r : type_rotation_relative;
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
								
								-- Derive the label rotation from the stub direction:
								r := to_label_rotation (stub.direction);
								t := (active => true, rotation => r, direction => direction, others => <>); -- CS size, ...
								segment.tag_labels.A := t;
							end if;

							if get_place (position) = get_B (segment) then
								-- Enable the tag label on the B end:
								log (text => "attach label to B end of segment", level => log_threshold + 2);
								
								-- Derive the label rotation from the stub direction:
								r := to_label_rotation (stub.direction);
								t := (active => true, rotation => r, direction => direction, others => <>); -- CS size, ...
								segment.tag_labels.B := t;
							end if;

							log_indentation_down;
							
						else
							--log (WARNING, "Net has no stub at" & to_string (position), console => true);
							log (text => "No stub found. No label placed.", level => log_threshold + 1);
						end if;

						-- signal iterations in upper level to cancel
						segment_found := true;						
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
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) &
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
		ports := ports_at_place (module_cursor, position, log_threshold + 1);
		
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

			when CAT_NET =>
				return to_string (object.net);

			when CAT_LABEL =>
				return to_string (object.label);
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
		result_net		: type_object_net;
		result_label	: type_object_label;

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

		

		-- SEARCH FOR THE FIRST NET LABEL:
		
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



		
		-- If nothing has been found then the category is CAT_VOID.


	<<end_of_search>>
		
		log_indentation_down;
		
		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_SEGMENT =>
				return (CAT_SEGMENT, result_segment);

			when CAT_NET =>
				return (CAT_NET, result_net);

			when CAT_LABEL =>
				return (CAT_LABEL, result_label);

				
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
				log (text => "net labels", level => log_threshold + 1);
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
			
			
		begin
			search_nets; -- addresses whole nets		
			search_net_segments;			
			search_net_labels;
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

			when CAT_NET =>
				modify_status (module_cursor, object.net, operation, log_threshold + 1);

			when CAT_LABEL =>
				modify_status (module_cursor, object.label, operation, log_threshold + 1);
				
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
		reset_proposed_nets (module_cursor, log_threshold + 1);
		reset_proposed_segments (module_cursor, log_threshold + 1);
		reset_labels (module_cursor, log_threshold + 1);
		
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

					
					procedure query_segment (seg : in out type_net_segment) is
						-- Find the zone at which the segment
						-- is being attacked:
						zone : type_line_zone := get_zone (seg, point_of_attack);
					begin
						log (text => to_string (zone), level => log_threshold);
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
					end query_segment;

					
				begin
					strand.segments.update_element (get_segment (object_cursor), query_segment'access);
				end query_strand;
				
										 
			begin				
				net.strands.update_element (get_strand (object_cursor), query_strand'access);
			end query_net;
			
			
		begin
			module.nets.update_element (get_net (object_cursor), query_net'access);
		end query_module;


		
	begin
		log (text => "module " & to_string (module_cursor)
			& " set A/B of primary segment moving " 
			& " point of attack " & to_string (point_of_attack),
			level => log_threshold);

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

			when CAT_NET => 
				null; -- CS

			when CAT_LABEL => 
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
			& " dragging object " 
			-- CS & to_string (object)
			& " to" & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_SEGMENT =>
				
				drag_segment (
					module_cursor	=> module_cursor,
					net_cursor		=> object.segment.net_cursor,
					strand_cursor	=> object.segment.strand_cursor,
					segment_cursor	=> object.segment.segment_cursor,
					point_of_attack	=> point_of_attack,
					coordinates		=> absolute,
					destination		=> destination,
					log_threshold	=> log_threshold + 1);


			when CAT_NET => 
				null; -- CS

			when CAT_LABEL => 
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
				null;
				-- CS
				

			when CAT_NET => 
				null; -- CS
				-- delete_net (
				-- 	module_cursor	=> module_cursor,
				-- 	log_threshold	=> log_threshold + 1);

				
			when CAT_LABEL => 
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

			when CAT_NET => 
				show_net (module_cursor, object.net.net_cursor, log_threshold + 1);

			when CAT_LABEL => 
				null; -- CS
				
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
