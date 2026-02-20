------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                   SCHEMATIC OPERATIONS / NETCHANGERS                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                -- 
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo: 


with ada.text_io;					use ada.text_io;
with ada.containers;

with ada.exceptions;				use ada.exceptions;
with et_string_processing;			use et_string_processing;

with et_net_segment;					use et_net_segment;
with et_net_ports;						use et_net_ports;
with et_net_strands;					use et_net_strands;
with et_schematic_ops_nets;
with et_netchanger_symbol_schematic;
with et_module;							use et_module;


package body et_schematic_ops_netchangers is


	use pac_net_name;
	
	

	procedure dragging_not_possible (
		port 		: in string;
		position	: in type_object_position) is
	begin
		log (ERROR, "port " & enclose_in_quotes (port) &
			 " is directly connected with other ports at" &
			to_string (position => position) &
			 ". Dragging not possible !",
			 console => true);
		raise constraint_error;
	end;




	procedure relative_rotation_invalid is begin
		log (ERROR, "Relative rotation must be in range" & 
			to_string (rotation_relative_min) &
			" .." & 
			to_string (rotation_relative_max),
			console => true
			);
		raise constraint_error;
	end;

	

	
	
	procedure netchanger_not_found (
		index : in type_netchanger_id) 
	is begin
		log (ERROR, "netchanger" & to_string (index) & " not found !");
		raise constraint_error;
	end;

	


	

	function port_connected (
		module	: in pac_generic_modules.cursor;
		port	: in et_netlists.type_port_netchanger)
		return boolean 
	is
		result : boolean := false; 
		-- to be returned. goes true on the first (and only) match.

		use et_nets;
		

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
				use pac_strands;
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_segments (strand : in type_strand) is
					use pac_net_segments;
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					procedure query_ports (segment : in type_net_segment) is begin
						result := is_connected (segment, port);
					end query_ports;
					
					
				begin
					while (not result) and (has_element (segment_cursor)) loop
						
						query_element (
							position	=> segment_cursor,
							process		=> query_ports'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;

				
			begin
				while (not result) and (has_element (strand_cursor)) loop

					query_element (
						position	=> strand_cursor,
						process		=> query_segments'access);
					
					next (strand_cursor);
				end loop;
			end query_strands;

			
		begin
			while (not result) and (has_element (net_cursor)) loop

				pac_nets.query_element (
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
			
		end query_nets;

		
	begin
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_nets'access);
		
		return result;
	end port_connected;



	


	
	




	
	procedure drag_net_segments (
		module			: in pac_generic_modules.cursor;	-- the module
		ports_before	: in type_netchanger_ports;	-- the old port positions
		ports_after		: in type_netchanger_ports;	-- the new port positions
		sheet			: in type_sheet;			-- the sheet to look at
		log_threshold	: in type_log_level) 
	is
		port_before, port_after : type_vector_model;
		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is

			use pac_nets;			
			net_cursor : pac_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				-- This flag goes true once port_before has been found the first time
				-- and affected end points of segments have been moved to port_after.
				drag_processed : boolean := false;
				
				procedure query_segments (strand : in out type_strand) is
					use pac_net_segments;

					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure change_segment (segment : in out type_net_segment) is 
					-- Changes the position of start or end point of a segment according to the drag point.
					begin -- change_segment
						log_indentation_up;
						
						-- if port sits on a start point of a segment -> move start point
						if get_A (segment) = port_before then
							log (text => "move segment start point from" & 
								to_string (get_A (segment)),
								level => log_threshold + 3);

							set_A (segment, port_after);

							log (text => "to" & 
								to_string (get_A (segment)),
								level => log_threshold + 3);

							-- signal iterations in upper level to cancel
							drag_processed := true;
						end if;

						
						-- if port sits on an end point of a segment -> move end point
						if get_B (segment) = port_before then
							log (text => "move segment end point from" & 
								to_string (get_B (segment)),
								level => log_threshold + 3);

							set_B (segment, port_after);

							log (text => "to" & 
								to_string (get_B (segment)),
								level => log_threshold + 3);
							
							-- signal iterations in upper level to cancel
							drag_processed := true;
						end if;
						
						log_indentation_down;
					end change_segment;
					
					
				begin -- query_segments
					log_indentation_up;

					-- Probe all segments of strand for port_before. This loop must not
					-- abort even if drag_processed goes true. Reason: All segements
					-- meeting here must be dragged.
					while segment_cursor /= pac_net_segments.no_element loop

						log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
						
						update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> change_segment'access);
						
						next (segment_cursor);
					end loop;

					-- Update strand position if any movement took place.
					if drag_processed then
						set_strand_position (strand);
					end if;
					
					log_indentation_down;
				end query_segments;


				use pac_strands;
				
				
			begin -- query_strands
				log_indentation_up;
				while strand_cursor /= pac_strands.no_element loop
					
					-- We pick out only the strands on the targeted sheet:
					if get_sheet (element (strand_cursor).position) = sheet then
						log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

						log_indentation_up;
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							level => log_threshold + 1);
					
						-- Iterate in segments of strand. If point sits on any segment
						-- the flag drag_processed goes true.
						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);
					
						log_indentation_down;
					end if;
					-- All segments of strand probed (and maybe moved).

					-- If the drag point has been processed, there is no need to look up
					-- other strands for port_before.
					if drag_processed then exit; end if;
					
					next (strand_cursor);
				end loop;

				log_indentation_down;
			end query_strands;

			
		begin -- query_nets
			while net_cursor /= pac_nets.no_element loop

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_nets;

		
	begin -- drag_net_segments
		log (text => "dragging net segments with netchangers on sheet" & 
			 to_string (sheet) & " ...", level => log_threshold);
		log_indentation_up;

		--------------
		port_before := ports_before.master;
		log (text => "probing port " & to_string (port_before), level => log_threshold + 1);

		port_after := ports_after.master;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		---------------
		port_before := ports_before.slave;
		log (text => "probing port " & to_string (port_before), level => log_threshold + 1);

		port_after := ports_after.slave;		
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		
		log_indentation_down;
	end drag_net_segments;




	
	

	

	



	
	

	procedure insert_ports (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id;
		ports			: in type_netchanger_ports;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level) 
	is


		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

			procedure probe_port (
				port : in type_vector_model; -- x/y
				name : in type_netchanger_port_name) -- master/slave
			is

				-- This flag goes true on the first match. It signals
				-- all iterations to cancel prematurely.
				port_processed : boolean := false;
					
				use pac_nets;
				net_cursor : pac_nets.cursor := module.nets.first;

				
				procedure query_strands (
					net_name	: in pac_net_name.bounded_string;
					net			: in out type_net) 
				is
					strand_cursor : pac_strands.cursor := net.strands.first;

					
					procedure query_segments (strand : in out type_strand) is
						use pac_net_segments;
						segment_cursor : pac_net_segments.cursor := strand.segments.first;

						
						procedure change_segment (segment : in out type_net_segment) is
							use et_netlists;
							-- CS log messages
						begin
							-- If port sits on the A or B end of the segment,
							-- then insert it at this end:
							if get_A (segment) = port then
								insert_netchanger_port (segment, A, (index, name));  -- 1,2,3, .. / master/slave

								-- signal iterations in upper levels to cancel
								port_processed := true;
							end if;

							
							if get_B (segment) = port then
								insert_netchanger_port (segment, B, (index, name));  -- 1,2,3, .. / master/slave

								-- signal iterations in upper levels to cancel
								port_processed := true;
							end if;
							
						end change_segment;
						

					begin -- query_segments
						log_indentation_up;

						-- On the first segment, where the port sits on, this loop ends prematurely.
						while not port_processed and segment_cursor /= pac_net_segments.no_element loop
							log (text => "probing " & to_string (segment_cursor), level => log_threshold + 4);
							
							pac_net_segments.update_element (
								container	=> strand.segments,
								position	=> segment_cursor,
								process		=> change_segment'access);

							next (segment_cursor);
						end loop;

						log_indentation_down;
					end query_segments;


					use pac_strands;

					
				begin -- query_strands
					log_indentation_up;
					
					while not port_processed and strand_cursor /= pac_strands.no_element loop

						-- We pick out only the strands on the targeted sheet:
						if get_sheet (element (strand_cursor).position) = sheet then
							log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 3);

							log_indentation_up;
							log (text => "strand " & to_string (position => element (strand_cursor).position),
								level => log_threshold + 3);

							update_element (
								container	=> net.strands,
								position	=> strand_cursor,
								process		=> query_segments'access);
						
							log_indentation_down;
						end if;
							
						next (strand_cursor);
					end loop;
					
					log_indentation_down;
				end query_strands;

				
			begin -- probe_port
				log_indentation_up;
				log (text => "at " & to_string (port), level => log_threshold + 2);
				
				while not port_processed and net_cursor /= pac_nets.no_element loop
					
					update_element (
						container	=> module.nets,
						position	=> net_cursor,
						process		=> query_strands'access);
				
					next (net_cursor);
				end loop;

				log_indentation_down;
			end probe_port;

			
		begin
			log (text => "master port", level => log_threshold + 1);
			probe_port (ports.master, MASTER);

			log (text => "slave port", level => log_threshold + 1);			
			probe_port (ports.slave, SLAVE);
		end query_module;

		
	begin
		log (text => "insert netchanger ports in nets on sheet "
			 & to_string (sheet),
			 level => log_threshold);
		
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end insert_ports;





	


	
	function next_netchanger_index (
		module_cursor	: in pac_generic_modules.cursor)
		return type_netchanger_id 
	is
		next_idx : type_netchanger_id; -- to be returned

		
		-- Searches for the lowest available index.
		procedure search_gap (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			cursor : pac_netchangers.cursor := module.netchangers.first;

			-- We start the search with index 1.
			index_expected : type_netchanger_id := type_netchanger_id'first;

			gap_found : boolean := false; -- goes true once a gap has been found

			use pac_netchangers;
			
		begin
			while cursor /= pac_netchangers.no_element loop
					
				if key (cursor) /= index_expected then -- we have a gap
					next_idx := index_expected;
					gap_found := true;
					exit;
				end if;

				index_expected := index_expected + 1;
				next (cursor);
			end loop;

			-- If no gap has been found, then the index is the latest expected index.
			if not gap_found then
				next_idx := index_expected;
			end if;			
		end search_gap;
		
		
	begin -- next_netchanger_index
		query_element (
			position	=> module_cursor,
			process		=> search_gap'access);
		
		return next_idx;
	end next_netchanger_index;

	
	



	

	function exists_netchanger (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		index			: in type_netchanger_id) -- 1, 2, 3, ...
		return boolean 
	is
		result : boolean := false; -- to be returned, goes true once the target has been found		

		
		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) is
			use pac_netchangers;
		begin -- query_netchangers
			if contains (module.netchangers, index) then
				result := true;
			end if;
		end query_netchangers;

		
	begin -- exists_netchanger
		query_element (
			position	=> module_cursor,
			process		=> query_netchangers'access);

		return result;
	end exists_netchanger;




	

	

	function get_netchanger_port_position (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in type_netchanger_id; -- 1,2,3,...
		port			: in type_netchanger_port_name; -- SLAVE/MASTER
		log_threshold	: in type_log_level)
		return type_object_position
	is
		port_position : type_object_position; -- to be returned		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being inquired

		
		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			nc_cursor : pac_netchangers.cursor;
			nc_position : type_object_position;
			port_xy : type_vector_model;

			use et_netchanger_symbol_schematic;
			use pac_netchangers;
		begin
			if contains (module.netchangers, index) then
				nc_cursor := find (module.netchangers, index); -- the netchanger should be there

				log_indentation_up;

				-- get netchanger position (sheet/x/y) and rotation in schematic
				nc_position := element (nc_cursor).position_sch;

				-- get the port position relative to the center of the netchanger
				case port is
					when MASTER =>
						port_xy := position_master_port_default;

					when SLAVE =>
						port_xy := position_slave_port_default;
				end case;

				-- Calculate the absolute port position in schematic by
				-- first rotating port_xy, and then moving port_xy:
				
				rotate_by (
					point		=> port_xy,
					rotation	=> get_rotation (nc_position));
				
				move_by (
					point	=> port_xy,
					offset	=> nc_position.place);

				-- Now port_xy holds the absolute x/y of the port in the schematic.

				-- Assemble the port_position to be returned:
				port_position := to_position (
					point	=> port_xy,
					sheet	=> get_sheet (nc_position)
					);
				
				log_indentation_down;				
			else
				netchanger_not_found (index);
			end if;
		end query_netchangers;

		
	begin -- position
		log (text => "module " & to_string (module_name) &
			 " locating netchanger " & to_string (index) & 
			 " port " &  to_string (port) & " ...", level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		query_element (
			position	=> module_cursor,
			process		=> query_netchangers'access);
		
		return port_position;
	end get_netchanger_port_position;


	



	
	
	procedure add_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in type_object_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			cursor : pac_netchangers.cursor;
			index : type_netchanger_id;
			netchanger : type_netchanger;
			inserted : boolean;
			ports : type_netchanger_ports;

			use pac_netchangers;
			
		begin

			-- Get the index to be used for the new netchanger:
			index := next_netchanger_index (module_cursor);
			
			log (text => "netchanger index is " & to_string (index),
				 level => log_threshold + 1);
			
			-- build the new netchanger
			netchanger.position_sch := place;

			-- insert the new netchanger in the module
			insert (
				container 	=> module.netchangers,
				key			=> index,
				new_item	=> netchanger,
				position	=> cursor,
				inserted	=> inserted -- CS not further evaluated. should always be true
				);

			-- Get the absolute positions of the netchanger ports according to 
			-- location and rotation in schematic.
			ports := netchanger_ports (cursor);

			-- Inserts the given netchanger ports in the net segments.
			insert_ports (
				module_cursor	=> module_cursor,
				index			=> index,
				ports			=> ports,
				sheet			=> get_sheet (place),
				log_threshold	=> log_threshold + 1);
			
		end query_module;

		
	begin
		log (text => "module " & to_string (module_name) 
			 & " add netchanger at " & to_string (position => place) 
			 & " rotation " & to_string (get_rotation (place)),
			level => log_threshold);

		log_indentation_up;
		
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;		
	end add_netchanger;





	
	
	procedure drag_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure movable_test (
		-- Tests whether the given netchanger ports of the netchanger at location 
		-- are movable. 
		-- The criteria for movement are: no device, no submodule ports there.
		-- The ports allowed here are the ports-to-be-dragged itself.
			location 			: in type_object_position; -- only sheet number matters
			netchanger_ports	: in type_netchanger_ports) -- x/y of master and slave port
		is			

			
			procedure test_point (
				point		: in type_object_position; -- sheet/x/y -- the point to be probed
				port_name	: in type_netchanger_port_name) -- master/slave
			is 
				use ada.containers;
				use et_netlists;
				ports : type_net_ports;
				port : type_port_netchanger;

				use pac_net_submodule_ports;
				use pac_device_ports;
				use pac_netchanger_ports;
				use et_schematic_ops_nets;
			begin
				-- If no net segments start or end at given point then this test won't
				-- complain. If segments are meeting this point, no other ports must be
				-- here (except the port-to-be-dragged):
				if net_segment_at_place (module_cursor, point) then

					-- There are net segments starting or ending at point.
					-- Make sure at point are no ports of devices, submodules or other 
					-- netchangers (except the submodule port to be dragged):

					port := (index, port_name); -- the port to be dragged, like netchanger 12 port master

					-- Collect all ports of possible other devices, submodules and netchangers
					-- at given point:
					ports := get_ports (module_cursor, point, log_threshold + 2);

					-- If no device and no submodule ports here:
					if is_empty (ports.devices) and is_empty (ports.submodules) then

						-- If the ONE and ONLY netchanger port is the 
						-- port-to-be-dragged then everything is fine.
						if length (ports.netchangers) = 1 then
							
							if contains (ports.netchangers, port) then
								null; -- fine -> movable test passed
							else
								-- there is another netchanger port
								dragging_not_possible (to_string (port_name), point);
							end if;
						
						else
							-- there are more submodule ports
							dragging_not_possible (to_string (port_name), point);
						end if;
						
					else -- device or netchanger ports here
						dragging_not_possible (to_string (port_name), point);
					end if;
				end if;
			end test_point;

			
		begin -- movable_test
			log (text => "movable test ...", level => log_threshold + 1);
			log_indentation_up;

			-- Test point where the master port is:
			test_point 
				(
				point		=> to_position (
								point => netchanger_ports.master,
								sheet => get_sheet (location)),
				port_name	=> MASTER
				);

			-- Test point where the slave port is:			
			test_point 
				(
				point		=> to_position (
								point => netchanger_ports.slave,
								sheet => get_sheet (location)),
				port_name	=> SLAVE
				);
		
			log_indentation_down;
		end movable_test;


		
		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			cursor : pac_netchangers.cursor;
			location : type_object_position;
			ports_old : type_netchanger_ports;
			ports_new : type_netchanger_ports;
			
			procedure move (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) 
			is begin
				netchanger.position_sch := location;
			end move;


			use pac_netchangers;

			
		begin -- query_netchangers

			-- locate given netchanger
			cursor := find (module.netchangers, index);

			if cursor /= pac_netchangers.no_element then 
				-- netchanger exists

				-- Before the actual drag, the coordinates of the
				-- netchanger ports must be fetched. These coordinates will later assist
				-- in changing the positions of connected net segments.
				ports_old := netchanger_ports (cursor);

				-- Fetch the netchanger position BEFORE the move.
				location := element (cursor).position_sch;

				-- Test whether the port at the current position can be dragged:
				movable_test (location, ports_old);
				
				-- calculate the new position the netchanger will have AFTER the move:
				case coordinates is
					when ABSOLUTE =>
						-- The absolute position is defined by the given point (x/y).
						-- The sheet number does not change.
						set_place (location, point);

					when RELATIVE =>
						-- The new relative position is the netchanger position BEFORE 
						-- the move operation shifted by the given point (x/y).
						-- The sheet number does not change.
						move_by (
							point		=> location.place,
							offset		=> point);
				end case;

				-- move the netchanger to the new position
				update_element (
					container	=> module.netchangers,
					position	=> cursor,
					process		=> move'access);

				-- Get the NEW absolute positions of the netchanger ports AFTER
				-- the move operation according to location and rotation in schematic.
				ports_new := netchanger_ports (cursor);

				-- Change net segments in the affected nets (type_generic_module.nets):
				drag_net_segments (
					module			=> module_cursor,
					ports_before	=> ports_old,
					ports_after		=> ports_new,
					sheet			=> get_sheet (location),
					log_threshold	=> log_threshold + 1);

				-- The drag operation might result in new port-to-net connections.
				-- So we must insert new ports in segments.
				-- Insert possible new netchanger ports in the nets (type_generic_module.nets):
				log_indentation_up;
				
				-- Inserts the netchanger ports in the net segments.
				insert_ports (
					module_cursor	=> module_cursor,
					index			=> index,
					ports			=> ports_new,
					sheet			=> get_sheet (location),
					log_threshold	=> log_threshold + 1);

				log_indentation_down;
			else
				-- netchanger does not exist
				netchanger_not_found (index);
			end if;
			
		end query_netchangers;

		
	begin -- drag_netchanger
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" dragging netchanger" & to_string (index) &
					" to" & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" dragging netchanger" & to_string (index) &
					" by" & to_string (point), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_netchangers'access);

	end drag_netchanger;



	

	


	procedure delete_ports (
		module			: in pac_generic_modules.cursor;			-- the module
		index			: in type_netchanger_id;	-- the netchanger id
		sheet			: in type_sheet;		-- the sheet where the netchanger is
		log_threshold	: in type_log_level) 
	is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_nets;
			net_cursor : pac_nets.cursor := module.nets.first;

			-- In order to speed up things we have two flags that indicate
			-- whether the master or slave port has been deleted from the nets.
			type type_deleted_ports is record
				master	: boolean := false;
				slave	: boolean := false;
			end record;

			deleted_ports : type_deleted_ports;

			
			-- This function returns true if master and slave port have been deleted.
			-- All iterations abort prematurely once all ports have been deleted.
			function all_ports_deleted return boolean is begin
				return deleted_ports.master and deleted_ports.slave;
			end;

			
			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_segments (strand : in out type_strand) is
					use pac_net_segments;
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure query_ports (segment : in out type_net_segment) is
						deleted : boolean := false;

						
						procedure delete_port is begin
							log (text => "sheet" & to_string (sheet) & " net " &
								to_string (key (net_cursor)) & " " &
								to_string (segment_cursor),
								level => log_threshold + 1);
						end;

						
					begin -- query_ports
						-- Search for the master port if it has not been deleted yet:
						if not deleted_ports.master then
							delete_port;
							delete_netchanger_port (segment, (index, MASTER), deleted);
							
							if deleted then
								deleted_ports.master := true;
							end if;
						end if;

						-- Search for the slave port if it has not been deleted yet:
						if not deleted_ports.slave then
							delete_port;
							delete_netchanger_port (segment, (index, SLAVE), deleted);
							
							if deleted then
								deleted_ports.slave := true;
							end if;
						end if;
					end query_ports;

					
				begin -- query_segments
					while not all_ports_deleted and segment_cursor /= pac_net_segments.no_element loop

						pac_net_segments.update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> query_ports'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;
				

				use pac_strands;

				
			begin -- query_strands
				while not all_ports_deleted and strand_cursor /= pac_strands.no_element loop

					if get_sheet (element (strand_cursor).position) = sheet then

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_segments'access);

					end if;
					
					next (strand_cursor);
				end loop;
			end query_strands;

			
		begin -- query_nets
			while not all_ports_deleted and net_cursor /= pac_nets.no_element loop

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
				
				next (net_cursor);
			end loop;

			-- CS: warning if all_ports_deleted still true ?
		end query_nets;

		
	begin
		log (text => "deleting netchanger ports in nets ...", level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end delete_ports;




	
	

	procedure move_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			cursor : pac_netchangers.cursor;
			location : type_object_position;
			ports : type_netchanger_ports;
			
			procedure move (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) is
			begin
				netchanger.position_sch := location;
			end move;


			use pac_netchangers;

			
		begin -- query_netchangers

			-- locate given netchanger
			cursor := find (module.netchangers, index);

			if cursor /= pac_netchangers.no_element then 
				-- netchanger exists

				-- Get coordinates of netchanger master port.
				-- Since the ports of a netchanger are all on the same sheet,
				-- the sheet is now provided by location.
				location := get_netchanger_port_position (
					module_name		=> module_name,
					index			=> index,
					port			=> MASTER,
					log_threshold	=> log_threshold + 1);

				-- CS this would be easier:
				-- location := element (cursor).position_sch;
				
				log_indentation_up;

				-- Delete netchanger ports in nets:
				delete_ports (
	 				module			=> module_cursor,
					index			=> index,

					-- Get sheet number from location:
					sheet			=> get_sheet (location),
					
					log_threshold	=> log_threshold + 1);

				-- calculate the new position 
				case coordinates is
					when ABSOLUTE =>
						-- The absolute position is defined by the given point (x/y) 
						-- and the given sheet number:
						location := to_position (point, type_sheet (sheet));

					when RELATIVE =>
						-- The relative position is the netchanger position BEFORE 
						-- the move operation shifted by the given point (x/y)
						-- and the given sheet number:
						location := element (cursor).position_sch;
						
						move (
							position	=> location,
							offset		=> to_position_relative (point, sheet));
				end case;

				-- move the netchanger to the new position
				update_element (
					container	=> module.netchangers,
					position	=> cursor,
					process		=> move'access);

				-- Get the NEW absolute positions of the netchanger ports AFTER
				-- the move operation according to location and rotation in schematic.
				ports := netchanger_ports (cursor);

				-- Inserts the netchanger ports in the net segments.
				insert_ports (
					module_cursor	=> module_cursor,
					index			=> index,
					ports			=> ports,
					sheet			=> get_sheet (location),
					log_threshold	=> log_threshold + 1);

				log_indentation_down;
			else
				-- netchanger does not exist
				netchanger_not_found (index);
			end if;
			
		end query_netchangers;

		
	begin -- move_netchanger
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving netchanger" & to_string (index) &
					" to sheet" & to_string (sheet) &
					to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving netchanger" & to_string (index) &
					" by " & relative_to_string (sheet) & " sheet(s)" &
					to_string (point), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_netchangers'access);

	end move_netchanger;




	



	procedure rotate_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id;
		rotation		: in et_schematic_geometry.type_rotation_model;
		log_threshold	: in type_log_level) 
	is

		use pac_netchangers;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			netchanger_cursor : pac_netchangers.cursor;

			-- The sheet where the netchanger is located
			-- in the schematic:
			sheet : type_sheet;

			ports_old : type_netchanger_ports;
			ports_new : type_netchanger_ports;

			
			procedure rotate (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) 
			is begin
				set_rotation_schematic (netchanger, rotation);
			end;
			
			
		begin
			-- Locate the requested netchanger in the
			-- given module. If it does not exist, then output
			-- a message:
			netchanger_cursor := get_netchanger (module.netchangers, index);

			
			if has_element (netchanger_cursor) then 
				-- netchanger exists

				-- Before the actual rotation, the current (old) coordinates 
				-- of the netchanger ports must be fetched:
				ports_old := netchanger_ports (netchanger_cursor);
			
				-- Get the sheet number where the netchanger is:
				sheet := get_sheet (netchanger_cursor);

				-- log sheet number:
				log (text => "found the netchanger on sheet " & to_string (sheet),
					 level => log_threshold + 1);
				
				-- Delete the old netchanger ports in connected
				-- net segments as they are BEFORE the rotation:
				delete_ports (
	 				module			=> module_cursor,
					index			=> index,
					sheet			=> sheet,
					log_threshold	=> log_threshold + 2);

				
				-- Rotate the netchanger to the new rotation:
				update_element (
					container	=> module.netchangers,
					position	=> netchanger_cursor,
					process		=> rotate'access);
				

				-- Get the NEW absolute positions of the netchanger ports AFTER
				-- the rotation:
				ports_new := netchanger_ports (netchanger_cursor);

				-- Inserts the new netchanger ports in the net segments:
				insert_ports (
					module_cursor	=> module_cursor,
					index			=> index,
					ports			=> ports_new,
					sheet			=> sheet,
					log_threshold	=> log_threshold + 2);

			else
				log (WARNING, " Netchanger " & to_string (index) & " not found !");
			end if;
		end query_module;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " rotate netchanger " & to_string (index) 
			& " to " & to_string (rotation), 
			level => log_threshold);

		log_indentation_up;

		
		-- Validate rotation. Must be 0 or 90, nothing else:
		if is_0_or_90 (rotation) then
		
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_module'access);

		else
			log (WARNING, " Rotation " & to_string (rotation) & " invalid !"
				 & " Must be either 0 or 90 degrees.");
		end if;

		
		log_indentation_down;
	end rotate_netchanger;

	


	


	procedure delete_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in type_netchanger_id; -- 1,2,3,...
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		
		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			cursor : pac_netchangers.cursor;
			location : type_object_position;

			use pac_netchangers;
		begin

			-- locate given netchanger
			cursor := find (module.netchangers, index);

			if cursor /= pac_netchangers.no_element then 
				-- netchanger exists

				-- Get coordinates of netchanger master port.
				-- Since the ports of a netchanger are all on the same sheet,
				-- the sheet is now provided by location.
				location := get_netchanger_port_position (
					module_name		=> module_name,
					index			=> index,
					port			=> MASTER,
					log_threshold	=> log_threshold + 1);

				log_indentation_up;

				-- Delete netchanger ports in nets:
				delete_ports (
	 				module			=> module_cursor,
					index			=> index,

					-- Get sheet number from location:
					sheet			=> get_sheet (location),
					
					log_threshold	=> log_threshold + 1);

				-- Delete the netchanger itself:
				delete (module.netchangers, cursor);
				
				log_indentation_down;
			else
				-- netchanger does not exist
				netchanger_not_found (index);
			end if;
		end query_netchangers;

		
	begin -- delete_netchanger
		log (text => "module " & to_string (module_name) &
			" deleting netchanger" & to_string (index),
			level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_netchangers'access);
		
		log_indentation_down;		
	end delete_netchanger;

	


	
end et_schematic_ops_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
