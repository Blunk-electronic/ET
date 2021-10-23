------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON NETS                          --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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

with ada.strings;					use ada.strings;
with ada.strings.unbounded;			use ada.strings.unbounded;
with ada.exceptions;

with et_exceptions;					use et_exceptions;

package body et_schematic_ops.nets is

	use et_symbols.pac_shapes;
	use et_symbols.pac_text;
	
	procedure junction_in_sloping_segment (point : in et_coordinates.type_position) is begin
		log (ERROR, "Junction not allowed in a sloping net segment at" & to_string (point),
			 console => true);
		raise constraint_error;
	end;

	
	function between_start_and_end_point (
		point 		: in type_point;
		segment 	: in pac_net_segments.cursor;
		catch_zone	: in type_catch_zone := type_catch_zone'first)
		return boolean 
	is
		dist : type_distance_point_line;
		use pac_net_segments;
	begin
		dist := get_distance (
			point 		=> point,
			line		=> element (segment),
			line_range	=> BETWEEN_END_POINTS);

		if (not out_of_range (dist)) and to_distance (get_distance (dist)) <= catch_zone then
			return true;
		else
			return false;
		end if;
	end between_start_and_end_point;

	
	function on_segment (
		point 		: in type_point;
		segment 	: in pac_net_segments.cursor;
		catch_zone	: in type_catch_zone := type_catch_zone'first)
		return boolean 
	is
		dist : type_distance_point_line;
		use pac_net_segments;
	begin
		dist := get_distance (
			point 		=> point,
			line		=> element (segment),
			line_range	=> WITH_END_POINTS);

-- 		log (text => 
-- 			"catch zone" & to_string (catch_zone) 
-- 			& " distance " & to_string (distance (dist))
-- 			& " out of range " & boolean'image (out_of_range (dist))
-- 			);
		
		if (not out_of_range (dist)) and to_distance (get_distance (dist)) <= catch_zone then
			return true;
		else
			return false;
		end if;
	end on_segment;
	
	
	procedure rename_net (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name_before	: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_name_after	: in pac_net_name.bounded_string; -- RESET_N, MOTOR_ON_OFF_N	
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_schematic.pac_nets;
		net_cursor_old : pac_nets.cursor; -- points to the old net
		net_cursor_new : pac_nets.cursor; -- points to the new net

		procedure create_net (
		-- Creates a new empty net named net_name_after. 
		-- Sets the cursor net_cursor_new to the new net.
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			inserted : boolean;
		begin
			insert (
				container	=> module.nets,
				key			=> net_name_after,

				-- The scope of the net assumes the default value LOCAL.
				-- CS: It could be reasonable to assume the scope of the old net.
				new_item	=> (others => <>),
				
				inserted	=> inserted,
				position	=> net_cursor_new
				);
		end create_net;
		
		procedure rename_everywhere (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			-- backup the old net
			net_old	: et_schematic.type_net := element (net_cursor_old);

			procedure copy_net_content (
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
			begin
				net := net_old;
			end copy_net_content;
			
		begin -- rename_everywhere
			
			-- delete the old net entirely:
			delete (
				container	=> module.nets,
				position	=> net_cursor_old);

			-- copy the old net to the new net:
			update_element (	
				container	=> module.nets,
				position	=> net_cursor_new,
				process		=> copy_net_content'access);
			
		end rename_everywhere;

		procedure rename_on_sheet (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			-- temporarily collection of strands
			use et_schematic.pac_strands;
			strands_on_sheet : et_schematic.pac_strands.list;
			
			procedure collect_strands (
			-- Collects all strands on the targeted sheet in container strands_on_sheet.
			-- Deletes the affected strands from the old net.
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_schematic.type_net) is

				strand_cursor : et_schematic.pac_strands.cursor := net.strands.first;
				strand : et_schematic.type_strand;
			begin
				log (text => "collecting strands of net " 
					 & to_string (net_name) & " ...", level => log_threshold + 1);
				
				log_indentation_up;
				
				-- Look at the strands that are on the targeted sheet.
				while strand_cursor /= pac_strands.no_element loop
					
					if sheet (element (strand_cursor).position) = sheet (place) then

						-- append strand to temporarily collection of strands on this sheet
						append (strands_on_sheet, element (strand_cursor));

						log (text => "strand at" & to_string (element (strand_cursor).position),
							 level => log_threshold + 2);

						-- delete strand in old net
						delete (net.strands, strand_cursor);
						
						-- strand_cursor points no to no_element.
						-- So we must reset it to the first strand in the net.
						-- This causes this loop to start anew.
						strand_cursor := net.strands.first;
					else
						next (strand_cursor);
					end if;
					
				end loop;

				log_indentation_down;
			end collect_strands;

			procedure move_strands (
			-- Adds the collection of strands strands_on_sheet 
			-- to the targeted net.
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
			begin
				splice (
					target => net.strands,
					source => strands_on_sheet,
					before => pac_strands.no_element);
			end;
			
		begin -- rename_on_sheet

			-- collect strands in old net
			update_element (
				container	=> module.nets,
				position	=> net_cursor_old,
				process		=> collect_strands'access);

			-- Issue warning if no strands have been collected. This can result:
			-- - from an attempt to rename on a sheet that does not exist 
			-- - from the fact that the targeted sheet does not contain the targeted net 
			if is_empty (strands_on_sheet) then
				log (WARNING, "no strands have been renamed on sheet" & to_sheet (sheet (place)) &
					 ". Check net name and sheet number !");

				-- A net without strands is useless. So the just created net must be discarded.
				log (text => "deleting net " & to_string (net_name_after), level => log_threshold + 1);
				delete (module.nets, net_cursor_new);
				
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
			module		: in out type_module) is

			use et_schematic.pac_strands;			
			strand_temp : et_schematic.type_strand;
			strand_found : boolean := false;

			procedure locate_strand (
			-- Locates the strand that starts at place and stores it in strand_temp.
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				strand_cursor : et_schematic.pac_strands.cursor := net.strands.first;
			begin
				-- Find the strand that starts at the given position.
				while strand_cursor /= pac_strands.no_element loop
					if element (strand_cursor).position = place then
						-- CS: if place is not exactly the start position of the strand,
						-- search for any other point on the strand instead.

						-- fetch strand from old net
						strand_temp := element (strand_cursor);

						-- delete strand in old net
						delete (net.strands, strand_cursor);

						strand_found := true;
						-- no need for further searching
						exit;
					end if;
					next (strand_cursor);
				end loop;
			end locate_strand;

			procedure move_strand (
			-- Moves strand_temp to the targeted net.
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
			begin
				append (net.strands, strand_temp);
			end;
			
		begin -- rename_strand

			-- locate the targeted strand and store it in strand_temp:
			update_element (
				container	=> module.nets,
				position	=> net_cursor_old,
				process		=> locate_strand'access);

			if not strand_found then
				log (WARNING, "strand not found at" & to_string (position => place) &
					 ". Check net name and position !");

				-- A net without strands is useless. So the just created net must be discarded.
				log (text => "deleting net " & to_string (net_name_after), level => log_threshold + 1);
				delete (module.nets, net_cursor_new);
				
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
		
		log (text => "module " & enclose_in_quotes (to_string (module_name)) &
			 " renaming net " & to_string (net_name_before) &
			 " to " & to_string (net_name_after),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

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
			log (text => "creating new net " & to_string (net_name_after), level => log_threshold + 1);

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> create_net'access);
		end if;
		-- Now net_cursor_new points to the new net.
		
		log_indentation_up;

		-- show where the renaming will be taking place:
		case scope is
			when EVERYWHERE =>
				log (text => "scope: everywhere -> all strands on all sheets", level => log_threshold);

				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> rename_everywhere'access);

			when SHEET =>
				log (text => "scope: all strands on sheet" & et_coordinates.to_sheet (sheet (place)), level => log_threshold);

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
		
		log_indentation_down;		
	end rename_net;

	procedure delete_net (
	-- Deletes a net. The scope determines whether to delete a certain strand,
	-- all strands on a certain sheet or on all sheets.
	-- CS If a particular strand on a sheet is to be deleted, the argument "place"
	-- must provide sheet and x/y start position of strand. In the future x/y can be
	-- any point on any segment of the strand. See comment in procedure locate_strand.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in type_net_scope; -- strand, sheet, everywhere
		place			: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_schematic.pac_nets;
		net_cursor : pac_nets.cursor; -- points to the net

		use et_schematic.pac_strands;
		
		procedure delete_everywhere (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
		begin
			delete (
				container	=> module.nets,
				position	=> net_cursor);
		end;

		procedure delete_on_sheet (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			procedure delete_strands_of_sheet (
			-- Removes the affected strands from the net.
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				strand_cursor : et_schematic.pac_strands.cursor := net.strands.first;
				strand_count_before : count_type := length (net.strands);
			begin
				-- Look at the strands that are on the targeted sheet.
				while strand_cursor /= pac_strands.no_element loop
					if sheet (element (strand_cursor).position) = sheet (place) then
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
					log (WARNING, "no strands have been deleted on sheet" & to_sheet (sheet (place)) &
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
			module		: in out type_module) is

			use et_schematic.pac_strands;			

			strand_found : boolean := false;

			procedure locate_strand (
			-- Locates the strand that starts at place.
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				strand_cursor : et_schematic.pac_strands.cursor := net.strands.first;
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
		
		log (text => "module " & to_string (module_name) &
			 " deleting net " & to_string (net_name),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

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
				log (text => "scope: all strands on sheet" & et_coordinates.to_sheet (sheet (place)), level => log_threshold);

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
		
		log_indentation_down;		
	end delete_net;
	
	procedure delete_segment (
	-- Deletes a segment of a net.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		place			: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_schematic.pac_nets;
		net_cursor : pac_nets.cursor; -- points to the net

		use et_schematic.pac_strands;

		procedure no_segment is begin
			log (WARNING, "segment not found at" & to_string (position => place) &
			 ". Check net name and position !");
		end;

		procedure query_net (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_strands (
			-- Searches the strands of the net for a segment that sits on given place.
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				strand_cursor : et_schematic.pac_strands.cursor := net.strands.first;
				segment_found, strand_found : boolean := false;

				use pac_net_segments;				
				
				procedure query_segments (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;
				begin
					while segment_cursor /= pac_net_segments.no_element loop

						-- If segment crosses the given x/y position (in place),
						-- delete the segment.
						if between_start_and_end_point (
							point	=> type_point (place),
							segment	=> segment_cursor) then

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
					
					if sheet (element (strand_cursor).position) = sheet (place) then

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
							null;
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
		log (text => "module " & to_string (module_name) &
			 " deleting in net " & to_string (net_name) &
			 " segment at" & to_string (position => place),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

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
		
		log_indentation_down;		
	end delete_segment;

	function no_ports (ports : in type_ports) return boolean is
		result : boolean := true;
		use pac_device_ports;
		use pac_submodule_ports;
		use et_netlists.pac_netchanger_ports;
	begin
		if length (ports.devices) > 0 then
			return false;
		end if;

		if length (ports.submodules) > 0 then
			result := false;
		end if;

		if length (ports.netchangers) > 0 then
			result := false;
		end if;

		return result;
	end no_ports;

	function movable (
		module_name		: in pac_module_name.bounded_string;
		segment			: in type_net_segment;
		zone			: in type_line_zone;
		point_of_attack	: in et_coordinates.type_position;
		log_threshold	: in type_log_level) 
		return boolean 
	is
		result : boolean := true; -- to be returned. true means the zone is movable.
		-- Goes false once a port has been found in the given zone.

		point : et_coordinates.type_position;

		procedure search_ports is
		-- Searches ports of devices, netchangers and submodules that sit on
		-- the point of interest.	
		-- On the first finding, sets result to false and finishes. If no 
		-- finding, result remains true.	
			use pac_device_ports;
			use pac_submodule_ports;

			use et_netlists;
			use pac_netchanger_ports;

			device : pac_device_ports.cursor := segment.ports_devices.first;
			submodule : pac_submodule_ports.cursor := segment.ports_submodules.first;
			netchanger : pac_netchanger_ports.cursor := segment.ports_netchangers.first;
		begin -- search_ports
			while device /= pac_device_ports.no_element loop

				if position ( -- CS use a similar function that takes only cursors ?
					module_name		=> module_name,
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

					if position ( -- CS use a similar function that takes only cursors ?
						module_name		=> module_name,
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

					if position ( -- CS use a similar function that takes only cursors ?
						module_name		=> module_name,
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
		
	begin -- movable
		log_indentation_up;
		
		-- The point of interest is on the sheet specified in argument "point_of_attack".
		-- The x/y coordinates are taken from the segment start or end point.
		
		case zone is
			when START_POINT =>
				point := to_position (
						point => segment.start_point,
						sheet => sheet (point_of_attack));

				search_ports; -- sets result to false if a port is connected with the start point
				
			when END_POINT =>
				point := to_position (
						point => segment.end_point,
						sheet => sheet (point_of_attack));

				search_ports; -- sets result to false if a port is connected with the end point
				
			when CENTER =>
				-- Both start and end point must be checked for any ports.
				-- First check the start point of the segment.
				-- If start point is movable, then the end point must be checked too.
				point := to_position (
						point => segment.start_point,
						sheet => sheet (point_of_attack));

				search_ports; -- sets result to false if a port is connected with the start point

				-- If start point is movable, check end point.
				if result = true then
					point := to_position (
							point => segment.end_point,
							sheet => sheet (point_of_attack));

					search_ports; -- sets result to false if a port is connected with the end point
				end if;
		end case;

		log_indentation_down;
		
		return result;
	end movable;

	procedure move_net_labels (
		segment_before	: in type_net_segment;
		segment_after	: in out type_net_segment;
		zone			: in type_line_zone)
	is 
		-- Calculate the displacement of the start and end point:
		
		delta_start : constant type_distance_relative :=
			get_distance_relative (segment_before.start_point, segment_after.start_point);
		
		delta_end	: constant type_distance_relative :=
			get_distance_relative (segment_before.end_point, segment_after.end_point);
															
		use pac_net_labels;
		label_cursor : pac_net_labels.cursor := segment_after.labels.first;

		procedure move (l : in out type_net_label) is begin
			-- The position of a net label is absolute.
			
			case l.appearance is
				when TAG => 
					-- Moving the tag labels is quite simple because
					-- they are always at start or end point.
					-- So the label position change is just the displacement
					-- of the start or end point:
					case zone is
						when START_POINT =>
							if l.position = segment_before.start_point then
								move_by (l.position, delta_start);
							end if;
							
						when END_POINT => 
							if l.position = segment_before.end_point then
								move_by (l.position, delta_end);
							end if;

						when CENTER =>
							if l.position = segment_before.start_point then
								move_by (l.position, delta_start);
							end if;

							if l.position = segment_before.end_point then
								move_by (l.position, delta_end);
							end if;
							
					end case;

					-- CS: change rotation of label ?
					
				when SIMPLE => null; -- CS
					-- This requires a bit more math because simple labels
					-- are mostly between start and end point.

					-- CS: change rotation of label ?
			end case;
		end move;
		
	begin -- move_net_labels
		while label_cursor /= pac_net_labels.no_element loop
			update_element (segment_after.labels, label_cursor, move'access);
			next (label_cursor);
		end loop;
		
	end move_net_labels;
	
	procedure drag_segment (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		point_of_attack	: in et_coordinates.type_position; -- sheet/x/y
		coordinates		: in type_coordinates; -- relative/absolute
		destination		: in type_point; -- x/y, the new position 
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_schematic.pac_nets;
		net_cursor : pac_nets.cursor; -- points to the net

		use et_schematic.pac_strands;

		procedure no_segment is begin
			log (WARNING, "No segment found at" & to_string (position => point_of_attack) &
			 ". Check net name and position !");
		end;
		
		procedure query_net (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			procedure query_strands (
			-- Searches the strands of the net for a segment that sits on given point_of_attack.
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
				strand_cursor : et_schematic.pac_strands.cursor := net.strands.first;
				segment_found, strand_found : boolean := false;

				use pac_net_segments;				
				
				procedure query_segments (strand : in out type_strand) is
					segment_cursor : pac_net_segments.cursor := strand.segments.first;
					segment_cursor_target : pac_net_segments.cursor;
					target_segment_before : type_net_segment;

					zone : type_line_zone;

					procedure move_targeted_segment (segment : in out type_net_segment) is 
						-- In case absolute movement is required we need these values:
						dx : constant type_distance := get_distance (type_point (point_of_attack), destination, X);
						dy : constant type_distance := get_distance (type_point (point_of_attack), destination, Y);

						-- backup the segment as it was before the move/drag:
						segment_before : constant type_net_segment := segment;
					begin
						case zone is
							when START_POINT =>
								case coordinates is
									when ABSOLUTE =>
										if dx = zero or dy = zero then

											move_by (
												point	=> segment.start_point,
												offset	=> to_distance_relative (set (dx, dy)));
										
										else
											segment.start_point := destination;
										end if;
										
									when RELATIVE =>
										move_by (
											point	=> segment.start_point,
											offset	=> to_distance_relative (destination));
								end case;
								
							when END_POINT =>
								case coordinates is
									when ABSOLUTE =>
										if dx = zero or dy = zero then

											move_by (
												point	=> segment.end_point,
												offset	=> to_distance_relative (set (dx, dy)));

										else
											segment.end_point := destination;
										end if;

									when RELATIVE =>
										move_by (
											point	=> segment.end_point,
											offset	=> to_distance_relative (destination));
								end case;

							when CENTER =>
								case coordinates is
									when ABSOLUTE =>
										move_by (
											point	=> segment.start_point,
											offset	=> to_distance_relative (set (dx, dy)));

										move_by (
											point	=> segment.end_point,
											offset	=> to_distance_relative (set (dx, dy)));

									when RELATIVE =>
										move_by (
											point	=> segment.start_point,
											offset	=> to_distance_relative (destination) -- the given position is relative
											);

										move_by (
											point	=> segment.end_point,
											offset	=> to_distance_relative (destination) -- the given position is relative
											);
										
								end case;
						end case;

						move_net_labels (
							segment_before	=> segment_before,
							segment_after	=> segment,
							zone			=> zone);
						
					end move_targeted_segment;

					procedure move_connected_segment (connected_segment : in out type_net_segment) is 
					-- This procedure moves the start/end points of segments that are connected
					-- with the target_segment_before.

						-- backup the segment as it was before the move/drag:
						segment_before : constant type_net_segment := connected_segment;
						
						procedure copy_start_point is begin
							if connected_segment.start_point = target_segment_before.start_point then
								
								-- The connected segment is being dragged at its start point:
								connected_segment.start_point := element (segment_cursor_target).start_point;

								move_net_labels (
									segment_before	=> segment_before,
									segment_after	=> connected_segment,
									zone			=> START_POINT);

							end if;

							if connected_segment.end_point = target_segment_before.start_point then
								
								-- The connected segment is being dragged at its end point:
								connected_segment.end_point := element (segment_cursor_target).start_point;

								move_net_labels (
									segment_before	=> segment_before,
									segment_after	=> connected_segment,
									zone			=> END_POINT);

							end if;
						end;

						procedure copy_end_point is begin
							if connected_segment.start_point = target_segment_before.end_point then

								-- The connected segment is being dragged at its start point:
								connected_segment.start_point := element (segment_cursor_target).end_point;

								move_net_labels (
									segment_before	=> segment_before,
									segment_after	=> connected_segment,
									zone			=> START_POINT);

							end if;

							if connected_segment.end_point = target_segment_before.end_point then
								-- The connected segment is being dragged at its end point:
								
								connected_segment.end_point := element (segment_cursor_target).end_point;

								move_net_labels (
									segment_before	=> segment_before,
									segment_after	=> connected_segment,
									zone			=> END_POINT);

							end if;
						end;
						
					begin -- move_connected_segment
						case zone is
							when START_POINT => 
								-- The segment start or end point moves to the targeted segment start point.
								copy_start_point; 
								
							when END_POINT => 
								-- The segment start or end point moves to the targeted segment end point.
								copy_end_point;
								
							when CENTER => 
								-- The segment start or end point moves to the targeted segment start point.
								copy_start_point; 

								-- The segment start or end point moves to the targeted segment end point.
								copy_end_point;
						end case;
					end move_connected_segment;

					procedure connect_ports (segment : in out type_net_segment) is
					-- Looks up ports of devices, netchangers or submodules that are 
					-- to be connected with the segment. The place where ports are
					-- searched depends on the zone that has been moved.
					-- (The given segment sits already at the new position.)
						ports : et_schematic_ops.type_ports;

						procedure append_portlists is 
						-- Append the portlists obtained from function ports_at_place
						-- to the segment.
						-- CS: Special threatment required if a port is among the portlists
						-- that is already somewhere in the strand. 
						-- This particular port must be exempted from the appending.
						-- Currently only the integrity check (procedure check_integrity)
						-- detects this rare case.
						begin
							pac_device_ports.union (segment.ports_devices, ports.devices);
							pac_submodule_ports.union (segment.ports_submodules, ports.submodules);
							et_netlists.pac_netchanger_ports.union (segment.ports_netchangers, ports.netchangers);
						end append_portlists;
						
					begin -- connect_ports
						case zone is
							when START_POINT =>
								ports := ports_at_place 
									(
									module_name	=> module_name, 
									place 		=> to_position (
													point => segment.start_point,
													sheet => sheet (point_of_attack)),
									log_threshold => log_threshold + 1
									);

								append_portlists;
								
							when END_POINT =>
								ports := ports_at_place 
									(
									module_name	=> module_name, 
									place 		=> to_position (
													point => segment.end_point,
													sheet => sheet (point_of_attack)),
									log_threshold => log_threshold + 1
									);

								append_portlists;
								
							when CENTER =>
								ports := ports_at_place 
									(
									module_name	=> module_name, 
									place 		=> to_position (
													point => segment.start_point,
													sheet => sheet (point_of_attack)),
									log_threshold => log_threshold + 1
									);

								append_portlists;
								
								ports := ports_at_place 
									(
									module_name	=> module_name, 
									place 		=> to_position (
													point => segment.end_point,
													sheet => sheet (point_of_attack)),
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
							point		=> type_point (point_of_attack),
							segment		=> segment_cursor,
							catch_zone	=> catch_zone_default)
						then
							--log (text => "point of attack sits on segment", level => log_threshold + 1);
							
							-- Calculate the zone of attack:
							zone := which_zone (
								point	=> point_of_attack,
								line	=> element (segment_cursor));

							-- depending on zone, drag start point, end point or both
							log (text => "dragging segment" 
								& to_string (element (segment_cursor))
								& " at " & type_line_zone'image (zone), level => log_threshold + 1);

							-- Test whether the zone is movable. If not movable, nothing happens.
							if movable (
								module_name, element (segment_cursor),
								zone, point_of_attack, log_threshold + 1)
							then
								
								-- Backup the cursor of the targeted segment.
								-- Backup the segment as it was BEFORE the dragging.
								-- They are required later.
								segment_cursor_target := segment_cursor;
								target_segment_before := element (segment_cursor);

								-- move the targeted segment
								et_schematic.pac_net_segments.update_element (
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

					if not segment_found then no_segment; end if;

					-- MOVE SEGMENTS CONNECTED WITH THE TARGETED SEGMENT. 
					-- Iterate in segments. skip targeted segment because it has been dragged
					-- already (see above).
					segment_cursor := strand.segments.first; -- reset segment cursor
					while segment_cursor /= pac_net_segments.no_element loop
						if segment_cursor /= segment_cursor_target then

							et_schematic.pac_net_segments.update_element (
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
					
				end query_segments;
				
			begin -- query_strands
				
				-- Look at strands that are on the given sheet. This loop ends prematurely
				-- as soon as a segment has been found.
				while not segment_found and strand_cursor /= pac_strands.no_element loop
					
					if sheet (element (strand_cursor).position) = sheet (point_of_attack) then

						-- signal the calling unit that a strand has been found:
						strand_found := true;

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);

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
			
		end query_net;
		
	begin -- drag_segment
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " 
					& enclose_in_quotes (to_string (module_name))
					& " dragging net " 
					& enclose_in_quotes (to_string (net_name))
					& " segment at" & to_string (position => point_of_attack)
					& " to" & to_string (destination), level => log_threshold);

			when RELATIVE =>
				log (text => "module " 
					& enclose_in_quotes (to_string (module_name))
					& " dragging net " 
					& enclose_in_quotes (to_string (net_name))
					& " segment at" & to_string (position => point_of_attack)
					& " by" & to_string (destination), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);

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
		
		log_indentation_down;		
	end drag_segment;

	function nets_at_place (
		module_name		: in pac_module_name.bounded_string;
		place			: in et_coordinates.type_position;
		log_threshold	: in type_log_level)
		return type_net_names.list 
	is
		nets : type_net_names.list; -- to be returned

		module_cursor : pac_generic_modules.cursor; -- points to the module
		use et_schematic.pac_nets;
		net_cursor : pac_nets.cursor; -- points to the net

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) is

			procedure query_nets (net_cursor : in pac_nets.cursor) is
				use pac_strands;
				net : type_net := element (net_cursor);

				-- once a segment has been found at place, this flag goes true:
				match : boolean := false;
				
				strand_cursor : pac_strands.cursor := net.strands.first;

				procedure query_segments (strand : in type_strand) is
					use pac_net_segments;
					segment_cursor : pac_net_segments.cursor := strand.segments.first;
				begin -- query_segments
					while segment_cursor /= pac_net_segments.no_element loop
						log (text => "segment " & to_string (segment_cursor), level => log_threshold + 2);

						if on_line (
							point 	=> type_point (place),
							line	=> element (segment_cursor)) then
						
							log (text => " match", level => log_threshold + 2);

							match := true; -- signals the calling unit to cancel the search

							-- store net name in return value
							type_net_names.append (nets, key (net_cursor));

							exit; -- no need to search for more segments in this strand
						end if;
						
						next (segment_cursor);
					end loop;
				end query_segments;
				
			begin -- query_nets
				-- Search in strands of net. Cancel search after the first matching segment.
				while not match and strand_cursor /= pac_strands.no_element loop

					-- Look at strands on the given sheet only:
					if sheet (element (strand_cursor).position) = sheet (place) then

						log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
						log_indentation_up;
						
						pac_strands.query_element (
							position	=> strand_cursor,
							process 	=> query_segments'access);

						log_indentation_down;
					end if;
					
					next (strand_cursor);
				end loop;
			end query_nets;				
			
		begin -- query_module
			iterate (module.nets, query_nets'access);
		end query_module;
		
	begin -- nets_at_place
		log (text => "module " & to_string (module_name) &
			 " locating nets at" & to_string (position => place),
			 level => log_threshold);

		log_indentation_up;

		-- locate module
		module_cursor := locate_module (module_name);

		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
		return nets;
	end nets_at_place;

	
	procedure insert_segment (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in out pac_nets.cursor;
		sheet			: in type_sheet;
		net_name		: in pac_net_name.bounded_string;
		segment_new		: in et_schematic.type_net_segment;
		log_threshold	: in type_log_level)
	is 
		use et_schematic.pac_nets;
		segment : type_net_segment := segment_new;
		point : et_coordinates.type_position;

		type type_junction is record
			required	: boolean := false;
			place		: et_coordinates.type_position;
		end record;

		junction_at_start_point : type_junction;
		junction_at_end_point	: type_junction;
		
		use type_net_names;
		net_names : type_net_names.list;
		
		function list_nets return string is 
		-- Returns the content of net_names in a single string.
			net_cursor : type_net_names.cursor := net_names.first;
			use ada.strings.unbounded;
			names : ada.strings.unbounded.unbounded_string;
		begin
			while net_cursor /= type_net_names.no_element loop
				names := names & to_string (element (net_cursor)) & space;
				next (net_cursor);
			end loop;
			return to_string (names);
		end;

		procedure collision (point : in et_coordinates.type_position) is begin
			raise semantic_error_1 with
				"ERROR: Net segment collides at" & to_string (position => point) 
				& " with net(s): " & list_nets & " !";
		end collision;
		
		ports : type_ports;
		
		procedure assign_ports_to_segment is begin
			pac_device_ports.union (segment.ports_devices, ports.devices);
			pac_submodule_ports.union (segment.ports_submodules, ports.submodules);
			et_netlists.pac_netchanger_ports.union (segment.ports_netchangers, ports.netchangers);
		end;

		
		procedure create_net (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is
			inserted : boolean;
			strand : type_strand;
			net : type_net;

			procedure evaluate_net_names (point : in et_coordinates.type_position) is 
			-- Issues error message and raises constraint_error if net_names contains
			-- any foreign net names.
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
					point => segment_new.start_point);
			
			net_names := nets_at_place (
					module_name		=> module_name,
					place			=> point,
					log_threshold	=> log_threshold);

			evaluate_net_names (point);
			
			-- Test whether any foreign nets cross the end point of the segment:
			point := to_position (
					sheet => sheet,
					point => segment_new.end_point);
			
			net_names := nets_at_place (
					module_name		=> module_name,
					place			=> point,
					log_threshold	=> log_threshold);

			evaluate_net_names (point);
			-------------
			


			
			-----------
			-- look for any ports at start point of the new net segment
			ports := ports_at_place (
					module_name		=> module_name,
					place			=> to_position (
										sheet => sheet,
										point => segment_new.start_point),
					log_threshold	=> log_threshold);

			assign_ports_to_segment;

			-- look for any ports at end point of the new net segment
			-- The end point is just x/y. The sheet must be derived from the start point.
			ports := ports_at_place (
					module_name		=> module_name,
					place			=> to_position (
										sheet => sheet,
										point => segment_new.end_point),
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
			module		: in out type_module) is

			attach_to_strand : boolean := false;

			procedure evaluate_net_names (point : in et_coordinates.type_position) is 
			-- Issues error message and raises constraint_error if net_names contains
			-- any net names but the given net_name.
			-- If net_names contains the given net_name, then the flag attach_to_strand
			-- is set. The strand will be extended later by the segment specified by 
			-- start_point and end_point.
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

			use pac_strands;
			
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
			
			function which_strand (place : in et_coordinates.type_position) 
			-- Returns a cursor to the strand at place and a flag whether to place
			-- a junction at the given place.
				return type_which_strand is

				result : type_which_strand; -- to be returned

				procedure query_strands (
				-- Searches strands of given net for a segment that crosses place.
				-- Cancels search on first match.
					net_name	: in pac_net_name.bounded_string;
					net			: in type_net) is
					segment_found : boolean := false;
					
					procedure query_segments (strand : in type_strand) is
					-- Iterate segments until first match.
						use pac_net_segments;
						segment_cursor : pac_net_segments.cursor := strand.segments.first;
					begin
						while segment_cursor /= pac_net_segments.no_element loop

							-- Test if place sits on segment.
							if on_line (
								point 	=> type_point (place),
								line	=> element (segment_cursor)) then

-- 								-- It is not allowed to place a junction in a sloped segment,
-- 								-- because splitting sloping segments seems a rare, difficult and dangerous task.
-- 								if segment_orientation (segment_cursor) = SLOPING then
-- 									junction_in_sloping_segment (place);
-- 								end if;

-- 								-- signal "strand iterator" to abort search prematurely
-- 								segment_found := true;
								
								-- test whether a junction is required at place
								if between_start_and_end_point (type_point (place), segment_cursor) then

									-- It is not allowed to place a junction in a sloped segment,
									-- because splitting sloping segments seems a rare, difficult and dangerous task.
									if segment_orientation (segment_cursor) = SLOPING then
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
						if et_coordinates.sheet (element (result.cursor).position) = et_coordinates.sheet (place) then

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
			
			procedure extend_strand_start (
			-- Locates the strand (indicated by strand_at_start)
			-- and appends the new segment to it. 
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) is
			begin
				pac_strands.update_element (
					container	=> net.strands,
					position	=> strand_at_start.cursor,
					process		=> append_segment'access);
			end extend_strand_start;

			procedure extend_strand_end (
			-- Locates the strand (indicated by strand_at_end)
			-- and appends the new segment to it. 
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) is
			begin
				pac_strands.update_element (
					container	=> net.strands,
					position	=> strand_at_end.cursor,
					process		=> append_segment'access);
			end extend_strand_end;
			
			procedure create_strand (
			-- Creates a new strand that contains the segment.
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) is
				strand : type_strand;
			begin				
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
				
			end create_strand;

			procedure merge_strands (
			-- Merges two strands indicated by strand_at_start and strand_at_end.
			-- The strand_at_start will merge into strand_at_end.
			-- strand_at_start will be gone in the end. All its segments will move to 
			-- strand_at_end.
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) is

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
					point => segment_new.start_point);

			net_names := nets_at_place (
					module_name		=> module_name,
					place			=> point,
					log_threshold	=> log_threshold);

			evaluate_net_names (point); -- modifies the attach_to_strand flag
			
			-- Obtain the names of nets that cross the END point of the segment:
			point := to_position (
					sheet => sheet,
					point => segment_new.end_point);
			
			net_names := nets_at_place (
					module_name		=> module_name,
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
									point	=> segment_new.start_point));

				-- The END point of the new segment could join a strand.
				-- Obtain the cursor to the strand that crosses the END point:
				strand_at_end := which_strand (to_position (
									sheet => sheet,
									point => segment_new.end_point));

				-- The new segment must not be a redundant connection inside a strand:
				if not redundant_connection then
				
					-- Determine whether a junction will be placed here later:
					if not dead_end (strand_at_start) then
						-- The start point will be connected with a strand:
						log (text => "with its start point at " & 
							to_string (position => to_position (
														sheet	=> sheet,
														point	=> segment_new.start_point)),
							level => log_threshold + 2);

						-- If required, prepare placing a junction at start point of segment.
						-- The junction will be placed later.
						if strand_at_start.junction_required then
							junction_at_start_point.required := true;
							junction_at_start_point.place := to_position (
															sheet	=> sheet,
															point	=> segment_new.start_point);
						end if;
					end if;

					-- collect ports at dead end or where a junction is to be placed:
					if dead_end (strand_at_start) or junction_at_start_point.required then
						-- look for any ports at start point of the new net segment
						ports := ports_at_place (
								module_name		=> module_name,
								place			=> to_position (
													sheet	=> sheet,
													point	=> segment_new.start_point),
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
										point => segment_new.end_point)
										),
							level => log_threshold + 2);

						-- If required, prepare placing a junction at end point of segment.
						-- The junction will be placed later.
						if strand_at_end.junction_required then
							junction_at_end_point.required := true;
							junction_at_end_point.place := to_position (
										sheet => sheet,
										point => segment_new.end_point);
						end if;
					end if;
					
					-- collect ports at dead end or where a junction is to be placed:
					if dead_end (strand_at_end) or junction_at_end_point.required then
						-- look for any ports at end point of the new net segment
						-- The end point is just x/y. The sheet must be derived from the start point.
						ports := ports_at_place (
								module_name		=> module_name,
								place			=> to_position (
													sheet => sheet,
													point => segment_new.end_point),
								log_threshold	=> log_threshold);

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
			if junction_at_start_point.required then
				place_junction (
					module_name		=> key (module_cursor), -- CS use place_junction which takes a cursor instead
					place			=> junction_at_start_point.place,
					log_threshold	=> log_threshold + 1);
			end if;

			if junction_at_end_point.required then
				place_junction (
					module_name		=> key (module_cursor), -- CS use place_junction which takes a cursor instead
					place			=> junction_at_end_point.place,
					log_threshold	=> log_threshold + 1);
			end if;

			
			log_indentation_down;
		end if;

	end insert_segment;

	
	procedure insert_net (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		start_point		: in et_coordinates.type_position; -- sheet/x/y
		end_point		: in type_point; -- x/y
		log_threshold	: in type_log_level) 
	is		
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_schematic.pac_nets;
		net_cursor : pac_nets.cursor; -- points to the net
		segment : type_net_segment;

	begin -- insert_net
		log (text => "module " & to_string (module_name) &
			" inserting net " & to_string (net_name) &
			" segment from" & to_string (position => start_point) &
			" to" & to_string (end_point), level => log_threshold);
		
		-- locate module
		module_cursor := locate_module (module_name);

		-- The net can be in the module already. Locate the requested net in the module.
		-- net_cursor will point to no_element if the net is not already there.
		net_cursor := locate_net (module_cursor, net_name);

		-- build the segment from given start and end point
		segment.start_point := type_point (start_point);
		segment.end_point := end_point;
		
		log_indentation_up;

		insert_segment (
			module_cursor, net_cursor, sheet (start_point),
			net_name, segment, log_threshold + 1);

		log_indentation_down;
		
	end insert_net;


	procedure set_net_class (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		net_class		: in et_pcb.pac_net_class_name.bounded_string; -- pwr
		log_threshold	: in type_log_level)
	is
		use et_schematic.pac_nets;
		net_cursor : pac_nets.cursor; -- points to the net

		module_cursor : pac_generic_modules.cursor; -- points to the module

		procedure query_module (
			name	: in pac_module_name.bounded_string;
			module	: in out type_module)
		is
			procedure set_class (
				name	: in pac_net_name.bounded_string;
				net		: in out type_net)
			is 
				use et_pcb.pac_net_class_name;
			begin
				if net.class = net_class then
					log (text => "Net already in class " 
							& enclose_in_quotes (to_string (net_class)),
						level => log_threshold + 1);
				else
					log (text => "Changing net class from "
						 & enclose_in_quotes (to_string (net.class)) 
						 & " to " & enclose_in_quotes (to_string (net_class)),
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
		log (text => "module " & to_string (module_name) 
			& " setting class of net " & enclose_in_quotes (to_string (net_name)) 
			& " to " & enclose_in_quotes (et_pcb.to_string (net_class)),
			level => log_threshold);
		
		-- locate module
		module_cursor := locate_module (module_name);

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
	-- Sets the scope of a net.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		scope			: in et_netlists.type_net_scope; -- local/global
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_schematic.pac_nets;
		net_cursor : pac_nets.cursor; -- points to the net

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			procedure set (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) is
			begin
				net.scope := scope;
			end set;
			
		begin -- query_nets
			pac_nets.update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> set'access);

		end query_nets;
		
	begin -- set_scope
		log (text => "module " & to_string (module_name) &
			" setting scope of net " & to_string (net_name) &
			" to" & et_netlists.to_string (scope),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- locate the net
		net_cursor := locate_net (module_cursor, net_name);

		if net_cursor /= pac_nets.no_element then

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_nets'access);

		else
			net_not_found (net_name);
		end if;
	end set_scope;

	procedure place_junction (
	-- Places a net junction at the given position.
	-- If the junction is to be placed between start and end point of a segment, then the segment 
	-- is split in two new segments with the junction between them.
	-- If there is no net segment at the given position, no junction is placed and warning issued.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in et_coordinates.type_position; -- sheet/x/y, rotation doesn't matter
		log_threshold	: in type_log_level) is
		use et_coordinates;
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being checked

		segment_found : boolean := false; -- goes true if a net segment has been found to place the junction at
	
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
			use pac_nets;
			net_cursor : pac_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) is
				use et_coordinates;
				
				use pac_strands;
				strand_cursor : pac_strands.cursor := net.strands.first;
				
				procedure query_segments (strand : in out type_strand) is
					use pac_net_segments;
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
						
						procedure update_device_ports is 
						-- Queries the positions of the device ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
							use pac_device_ports;
							use et_symbols;

							procedure query_ports (cursor : in pac_device_ports.cursor) is
								device_name 	: type_device_name; -- IC23
								port_name		: pac_port_name.bounded_string; -- CE
								port_position 	: type_point; -- the xy-position of the port
							begin
								device_name	:= element (cursor).device_name;
								port_name	:= element (cursor).port_name;

								-- locate the port by module, device and port name:
								port_position := type_point (position (module_name, device_name, port_name, log_threshold + 1));
								log_indentation_up;
								
								log (text => "device " & to_string (device_name) & " port " & to_string (port_name) &
									" at" & to_string (port_position),
									level => log_threshold + 1);

								-- If the port was at the start point of the old segment, then
								-- it goes into segment_1.
								if port_position = old_segment.start_point then
									insert (segment_1.ports_devices, element (cursor));

								-- If the port was at the end point of the old segment, then
								-- it goes into segment_2.
								elsif port_position = old_segment.end_point then
									insert (segment_2.ports_devices, element (cursor));

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
							
							iterate (old_segment.ports_devices, query_ports'access);
							log_indentation_down;
						end update_device_ports;

						procedure update_submodule_ports is 
						-- Queries the positions of the submodule ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
							use pac_submodule_ports;

							procedure query_ports (cursor : in pac_submodule_ports.cursor) is
								submod_name 	: et_general.pac_module_instance_name.bounded_string; -- MOT_DRV_3
								port_name		: pac_net_name.bounded_string; -- RESET
								port_position 	: type_point; -- the xy-position of the port
							begin
								submod_name	:= element (cursor).module_name; -- CLOCK_GENERATOR
								port_name	:= element (cursor).port_name;	-- RESET

								-- locate the port by module, submodule and port name:
								port_position := type_point (position (module_name, submod_name, port_name, log_threshold + 1));
								log_indentation_up;
								
								log (text => "submodule " & to_string (submod_name) & " port " & to_string (port_name) &
									" at" & to_string (port_position),
									level => log_threshold + 1);

								-- If the port was at the start point of the old segment, then
								-- it goes into segment_1.
								if port_position = old_segment.start_point then
									insert (segment_1.ports_submodules, element (cursor));

								-- If the port was at the end point of the old segment, then
								-- it goes into segment_2.
								elsif port_position = old_segment.end_point then
									insert (segment_2.ports_submodules, element (cursor));

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
							
							iterate (old_segment.ports_submodules, query_ports'access);
							log_indentation_down;
						end update_submodule_ports;

						procedure update_netchanger_ports is 
						-- Queries the positions of the netchanger ports in the old_segment. 
						-- By the position assigns the ports to the new segments. 
							use et_netlists;
							use et_netlists.pac_netchanger_ports;
							use et_submodules;

							procedure query_ports (cursor : in pac_netchanger_ports.cursor) is
								index			: type_netchanger_id; -- 1,2,3,...
								port			: type_netchanger_port_name; -- SLAVE/MASTER
								port_position 	: type_point; -- the xy-position of the port
							begin
								index := element (cursor).index;
								port := element (cursor).port;

								-- locate the port by module, netchanger index and port:
								port_position := type_point (position (module_name, index, port, log_threshold + 1));
								log_indentation_up;
								
								log (text => "netchanger " & to_string (index) & " port " & to_string (port) &
									" at" & to_string (port_position),
									level => log_threshold + 1);

								-- If the port was at the start point of the old segment, then
								-- it goes into segment_1.
								if port_position = old_segment.start_point then
									insert (segment_1.ports_netchangers, element (cursor));

								-- If the port was at the end point of the old segment, then
								-- it goes into segment_2.
								elsif port_position = old_segment.end_point then
									insert (segment_2.ports_netchangers, element (cursor));

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
							
							iterate (old_segment.ports_netchangers, query_ports'access);
							log_indentation_down;
						end update_netchanger_ports;
						
					begin -- insert_two_new_segments
						-- set start and end points of new segments
						segment_1.start_point := old_segment.start_point;
						segment_1.end_point := type_point (place);
						segment_2.start_point := type_point (place);
						segment_2.end_point := old_segment.end_point;

						-- set junctions
						segment_1.junctions.start_point := old_segment.junctions.start_point;
						segment_1.junctions.end_point := true; -- because there is the new junction
						segment_2.junctions.start_point := false; -- no need for another junction at the same place
						segment_2.junctions.end_point := old_segment.junctions.end_point;

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

					procedure junction_at_start_point (segment : in out type_net_segment) is begin
						segment.junctions.start_point := true;
					end;

					procedure junction_at_end_point (segment : in out type_net_segment) is begin
						segment.junctions.end_point := true;
					end;
					
				begin -- query_segments
					while segment_cursor /= pac_net_segments.no_element loop

						-- The junction can be placed at the start or end point of a segment OR
						-- between start and end point of a segment. If none of these conditions
						-- is positive, go to next segment.
						
						--log_indentation_up;
						--log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);

						if type_point (place) = element (segment_cursor).start_point then

							-- place junction at start point of segment
							update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> junction_at_start_point'access);

							segment_found := true;
							exit; -- no need to search for other segments
							
						elsif type_point (place) = element (segment_cursor).end_point then

							-- place junction at end point of segment
							update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> junction_at_end_point'access);

							segment_found := true;
							exit; -- no need to search for other segments
							
						elsif between_start_and_end_point (
							point	=> type_point (place),
							segment	=> segment_cursor) then -- targeted segment found

							log (text => "net " & to_string (net_name) & " strand" &
								 to_string (position => strand.position), level => log_threshold + 1);
							log (text => to_string (segment_cursor), level => log_threshold + 1);

							-- Backup properties of old segment (it provides information on labels, ports and junctions):
							old_segment := element (segment_cursor);
							old_segment_orientation := segment_orientation (segment_cursor);

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
					if sheet (element (strand_cursor).position) = sheet (place) then
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

	begin -- place_junction
		log (text => "module " & to_string (module_name) & " placing junction at" &
			 to_string (position => place) & " ...", level => log_threshold);
		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_nets'access);

		if not segment_found then
			log (WARNING, "attempt to place junction in the void. Junction not placed !");
		end if;
		
		log_indentation_down;
	end place_junction;
	
	procedure place_net_label (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		segment_position: in et_coordinates.type_position; -- sheet/x/y
		label_position	: in type_point := origin; -- x/y
		rotation		: in et_coordinates.type_rotation := zero_rotation; -- 0, 90, 180. Relevant for simple labels only.
		appearance 		: in type_net_label_appearance; -- simple/tag label
		direction		: in et_schematic.type_net_label_direction; -- INPUT, OUTPUT, PASSIVE, ...
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_schematic.pac_nets;
		net_cursor : pac_nets.cursor; -- points to the net

		function no_label_placed return string is begin
			return (et_coordinates.to_string (position => segment_position) & " !" &
				" No label placed ! Specify another position and try again.");
		end;
		
		use type_net_names;
		nets : type_net_names.list;
		net_name : pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			-- This flag goes true once the first segment of the targeted net at
			-- the targeted sheet has been found.
			segment_found : boolean := false; -- to be returned

			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) is
				use et_coordinates;
				use pac_strands;
				strand_cursor : pac_strands.cursor := net.strands.first;
				
				procedure query_segments (strand : in out type_strand) is
					use pac_net_segments;

					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					procedure attach_label (segment : in out type_net_segment) is 
						use pac_net_labels;
						label : type_net_label_base;
					begin
						-- label_position is relative to segment_position
						label.position := label_position;
						move_by (label.position, to_distance_relative (segment_position));
						-- now label.position is absolute
						
						-- CS: label size, style and line width assume default. could be provided by further
						-- parameters passed to procedure place_net_label.

						case appearance is
							when SIMPLE =>
								append (
									container	=> segment.labels,
									new_item	=> (label with
										appearance		=> SIMPLE,

										-- snap given rotation to either 0 or 90 degree
										rotation_simple	=> snap (rotation))
									   );
								
							when TAG =>
								-- A tag label can be attached to a stub only.
								declare
									s : constant type_stub := query_stub (module_name, net_name, segment_position, log_threshold + 1);
								begin
									if s.is_stub then
									
										append (
											container	=> segment.labels,
											new_item	=> (label with 
												appearance		=> TAG,

												-- derive the label rotation from the stub direction:
												rotation_tag	=> to_label_rotation (s.direction),
															
												direction		=> direction) -- the given signal direction
										   );
										
									else
										log (WARNING, "Net has no stub at" & no_label_placed, console => true);
									end if;
								end;
						end case;
					end attach_label;
					
				begin -- query_segments
					while not segment_found and segment_cursor /= pac_net_segments.no_element loop

						if on_line (
							point 	=> type_point (segment_position),
							line	=> element (segment_cursor)) then
							
							update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> attach_label'access);

							-- signal iterations in upper level to cancel
							segment_found := true;
						end if;
						
						next (segment_cursor);
					end loop;
				end query_segments;
				
			begin -- query_strands
				while not segment_found and strand_cursor /= pac_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if et_coordinates.sheet (element (strand_cursor).position) = sheet (segment_position) then

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
					end if;
					
					next (strand_cursor);
				end loop;
			end query_strands;
			
		begin -- query_nets
			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> query_strands'access);

		end query_nets;
				
	begin -- place_net_label
		log (text => "module " & enclose_in_quotes (to_string (module_name)) &
			" labeling segment at"  &
			to_string (position => segment_position) &
			" with " & to_string (appearance) & " label at" &
			to_string (point => label_position) &
			" rotation" & to_string (rotation),
			level => log_threshold);
		-- CS rework. log message does not need rotation in case of tag label.
		
		log_indentation_up;

		-- locate module
		module_cursor := locate_module (module_name);

		-- collect names of nets that cross the given segment_position
		nets := nets_at_place (module_name, segment_position, log_threshold + 1);

		case length (nets) is
			when 0 =>
				log (WARNING, "no net found at" & no_label_placed);

			when 1 => 
				net_name := element (nets.first);
				log (text => "net name " & to_string (net_name), level => log_threshold + 1);
				
				-- Set the cursor to the net.
				net_cursor := locate_net (module_cursor, net_name);

				--log (text => "net name " & to_string (key (net_cursor)), level => log_threshold + 1);
				
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> query_nets'access);

			when others =>
				log (WARNING, "more than one net found at" & no_label_placed);
				-- CS show the net names
				
		end case;
		
		log_indentation_down;		
		
	end place_net_label;

	procedure delete_net_label (
	-- Deletes a label.
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		position		: in et_coordinates.type_position; -- sheet/x/y
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		-- This flag goes true once the targeted net label
		-- has been found. All iterations are cancelled as soon as it goes true.
		label_found : boolean := false;
		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is

			use pac_nets;
			net_cursor : pac_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) is
				use et_coordinates;
				use pac_strands;
				strand_cursor : pac_strands.cursor := net.strands.first;
				
				procedure query_segments (strand : in out type_strand) is
					use pac_net_segments;

					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					procedure query_labels (segment : in out type_net_segment) is 
						use pac_net_labels;
						label_cursor : pac_net_labels.cursor := segment.labels.first;
					begin
						while label_cursor /= pac_net_labels.no_element loop

							-- If label sits at position, delete it from the label list
							-- of that segment:
							if element (label_cursor).position = type_point (position) then
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
					if et_coordinates.sheet (element (strand_cursor).position) = sheet (position) then

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
		log (text => "module " & enclose_in_quotes (to_string (module_name)) &
			" deleting net label at" &
			et_coordinates.to_string (position => position),
			level => log_threshold);
		
		log_indentation_up;

		-- locate module
		module_cursor := locate_module (module_name);

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
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		net_name		: in pac_net_name.bounded_string; -- RESET, MOTOR_ON_OFF
		position		: in et_coordinates.type_position; -- sheet/x/y		
		log_threshold	: in type_log_level)
		return type_stub 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module
		net_cursor : pac_nets.cursor; -- points to the net
		
		use et_coordinates;
		use pac_nets;
		use pac_strands;

		ports : type_ports;
		
		stub_found : boolean := false;
		direction : type_stub_direction;
		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net) is
			
			strand_cursor : pac_strands.cursor := net.strands.first;

			procedure query_segments (strand : in type_strand) is
				use pac_net_segments;
				segment_cursor : pac_net_segments.cursor := strand.segments.first;

				segment_counter : natural := 0;

				procedure probe_direction is
					-- Get the stub direction. If the segment is sloped then it
					-- does not qualify as stub.
					s : constant type_stub := stub_direction (segment_cursor, type_point (position));
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
					if element (segment_cursor).start_point = type_point (position) then
						log (text => "match with start point of a segment", level => log_threshold + 2);
						probe_direction;
						
					elsif element (segment_cursor).end_point = type_point (position) then
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
				if element (strand_cursor).position.sheet = sheet (position) then

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
		ports := ports_at_place (module_name, position, log_threshold + 1);
		
		-- If there are no ports then examine the net further.
		-- If there are devices, submodule or netchanger ports, then the given position
		-- is definitely not a stub.
		if no_ports (ports) then
			log (text => "no ports here. examining net further ...", level => log_threshold + 1);
		
			-- locate module
			module_cursor := locate_module (module_name);

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
	
end et_schematic_ops.nets;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
