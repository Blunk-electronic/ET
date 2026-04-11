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


with ada.text_io;						use ada.text_io;
with ada.containers;

with ada.exceptions;					use ada.exceptions;
with et_string_processing;				use et_string_processing;

with et_net_segment;					use et_net_segment;
with et_net_ports;						use et_net_ports;
with et_net_ports_devices;				use et_net_ports_devices;
with et_net_strands;					use et_net_strands;
with et_schematic_ops_nets;
with et_schematic_ops_groups;
with et_schematic_ops_sheets;

with et_board_ops_ratsnest;
with et_netchanger_symbol_schematic;
with et_module;							use et_module;
with et_netchangers.schematic;			use et_netchangers.schematic;

with et_modes.schematic;
with et_undo_redo;
with et_commit;



package body et_schematic_ops_netchangers is


	use pac_net_name;
	use pac_netchangers;
	
	

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
		log (WARNING, "Netchanger " 
			& to_string (index) & " not found !");
	end;

	



	function get_netchangers (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_netchanger_ids.list
	is
		result : pac_netchanger_ids.list;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is 

			procedure query_netchanger (
				c : pac_netchangers.cursor) 
			is 
				index : type_netchanger_id renames key (c);
			begin
				result.append (index);
			end;

			
		begin
			module.netchangers.iterate (query_netchanger'access);
		end;


	begin
		query_element (module_cursor, query_module'access);
		
		return result;
	end get_netchangers;


	

	
	
	
	
	function get_first_netchanger (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_netchangers.cursor
	is
		result : pac_netchangers.cursor;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			result := module.netchangers.first;
		end;
		
		 
	begin
		query_element (module_cursor, query_module'access);

		return result;
	end get_first_netchanger;


	
	
	
	
	
	

	function get_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id)
		return pac_netchangers.cursor
	is 
		result : pac_netchangers.cursor;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is begin
			result := get_netchanger (module.netchangers, index);
		end;
		
		 
	begin
		query_element (module_cursor, query_module'access);

		return result;
	end get_netchanger;
	
	
	


	
	function get_netchanger_position (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id) -- 1,2,3,...
		return type_object_position
	is
		result : type_object_position;
		netchanger_cursor : pac_netchangers.cursor;
	begin
		netchanger_cursor := get_netchanger (module_cursor, index);
		
		result := to_object_position (
			get_position_schematic (netchanger_cursor));
		
		return result;
	end;
	
	
	
	
	
	

	function port_connected (
		module_cursor	: in pac_generic_modules.cursor;
		port			: in et_netlists.type_port_netchanger)
		return boolean 
	is
		-- CS rework, clean up
		
		
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
			position	=> module_cursor,
			process		=> query_nets'access);
		
		return result;
	end port_connected;



	


	
	
	procedure delete_ports (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id;
		-- CS use cursor to Netchanger instead of the index ?
		sheet			: in type_sheet; -- the sheet where the netchanger is
		log_threshold	: in type_log_level) 
	is
		-- CS rework, clean up
		
		
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
		log (text => "module " & to_string (module_cursor)
			 & " delete netchanger ports in nets",
			 level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_nets'access);

		log_indentation_down;
	end delete_ports;

	
	



	
	
	

	procedure insert_ports (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id;
		ports			: in type_netchanger_ports;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level) 
	is
		-- CS rework, clean up

		
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

	
	
	
	




	
	procedure drag_net_segments (
		module_cursor	: in pac_generic_modules.cursor;
		ports_before	: in type_netchanger_ports;
		ports_after		: in type_netchanger_ports;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level) 
	is
		-- CS rework, clean up
		port_before, port_after : type_vector_model;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is

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
				
				procedure query_segments (
					strand : in out type_strand) 
				is
					use pac_net_segments;

					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					-- Changes the position of start or end point of a segment according to the drag point.
					procedure change_segment (
						segment : in out type_net_segment) 
					is begin
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

			
		begin
			while has_element (net_cursor) loop

				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
		end query_module;

		
	begin -- drag_net_segments
		log (text => "module " & to_string (module_cursor)
			 & " drag net segments with netchangers on sheet " 
			 & to_string (sheet) & " ...",
			 level => log_threshold);
		
		log_indentation_up;

		--------------
		port_before := ports_before.master;
		log (text => "probing port " & to_string (port_before), level => log_threshold + 1);

		port_after := ports_after.master;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		---------------
		port_before := ports_before.slave;
		log (text => "probing port " & to_string (port_before), level => log_threshold + 1);

		port_after := ports_after.slave;		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		
		log_indentation_down;
	end drag_net_segments;




	
	

	


	
	function get_next_netchanger_index (
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
		
		
	begin
		query_element (
			position	=> module_cursor,
			process		=> search_gap'access);
		
		return next_idx;
	end get_next_netchanger_index;

	
	



	
	

	function netchanger_exists (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id) -- 1, 2, 3, ...
		return boolean 
	is
		-- CS rework, clean up
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

		
	begin -- netchanger_exists
		query_element (
			position	=> module_cursor,
			process		=> query_netchangers'access);

		return result;
	end netchanger_exists;




	
	
	
	
	function get_connected_net (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id;
		port			: in type_netchanger_port_name) -- SLAVE/MASTER
		return pac_nets.cursor
	is
		use pac_nets;
		result : pac_nets.cursor;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is			
			net_cursor : pac_nets.cursor := module.nets.first;
			
			use et_netlists;
			ports : pac_netchanger_ports.set;
			proceed : boolean := true;
		begin
			-- Iterate the nets of the module and abort
			-- as soon as a net has been found that contains
			-- the given netchanger and port.
			-- The cursor to the affected net is returned:
			while has_element (net_cursor) and proceed loop
				
				ports := get_netchanger_ports (net_cursor);
				
				if contains_netchanger_port (ports, index, port) then
					result := net_cursor;
					exit;
				end if;
				
				next (net_cursor);
			end loop;			
		end query_module;
		
		
	begin
		query_element (module_cursor, query_module'access);	
		return result;
	end get_connected_net;

	
	
	
	
	

	

	function get_netchanger_port_position (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		port			: in type_netchanger_port_name; -- SLAVE/MASTER
		log_threshold	: in type_log_level)
		return type_object_position
	is
		port_position : type_object_position; -- to be returned		
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			nc_cursor : pac_netchangers.cursor;
			nc_position : type_netchanger_position_schematic;
			port_xy : type_vector_model;

			use et_netchanger_symbol_schematic;
			use pac_netchangers;
		begin
			if contains (module.netchangers, index) then
				nc_cursor := find (module.netchangers, index); -- the netchanger should be there

				log_indentation_up;

				-- get netchanger position (sheet/x/y) and rotation in schematic
				nc_position := get_position_schematic (nc_cursor);

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
					offset	=> get_place (nc_position));

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
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " locate netchanger " & to_string (index) 
			 & " port " &  to_string (port),
			 level => log_threshold);

		query_element (
			position	=> module_cursor,
			process		=> query_module'access);
		
		return port_position;
	end get_netchanger_port_position;


	



	
	
	procedure add_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		place			: in type_object_position; -- sheet/x/y/rotation
		-- CS rename to position	
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level) 
	is
		use et_board_ops_ratsnest;
		use et_commit;
		use et_undo_redo;
		use et_modes.schematic;
		
		
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
			index := get_next_netchanger_index (module_cursor);
			
			log (text => "netchanger index is " & to_string (index),
				 level => log_threshold + 1);
			
			-- build the new netchanger
			set_position (netchanger, to_netchanger_position (place));

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
			ports := get_netchanger_ports (cursor);

			-- Inserts the given netchanger ports in the net segments.
			insert_ports (
				module_cursor	=> module_cursor,
				index			=> index,
				ports			=> ports,
				sheet			=> get_sheet (place),
				log_threshold	=> log_threshold + 1);
			
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " add netchanger at " & to_string (position => place) 
			 & " rotation " & to_string (get_rotation (place)),
			level => log_threshold);

		log_indentation_up;
		
		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold);
		end if;
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		
		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold);
		end if;
		
		
		update_ratsnest (module_cursor, log_threshold + 1);
				
		log_indentation_down;		
	end add_netchanger;




	
	
	
	
	
	procedure movable_test (
		module_cursor		: in pac_generic_modules.cursor;
		index				: in type_netchanger_id;
		location 			: in type_object_position; -- only sheet number matters
		netchanger_ports	: in type_netchanger_ports; -- x/y of master and slave port
		log_threshold		: in type_log_level)
	is			
		-- CS: clean up, rework
		
		
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
				ports := get_ports (module_cursor, point, log_threshold + 1);

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

		
	begin
		log (text => "movable test", level => log_threshold);
		log_indentation_up;

		-- Test the point where the master port is:
		test_point (
			point		=> to_position (
							point => netchanger_ports.master,
							sheet => get_sheet (location)),
			port_name	=> MASTER);

			
		-- Test the point where the slave port is:			
		test_point (
			point		=> to_position (
							point => netchanger_ports.slave,
							sheet => get_sheet (location)),
			port_name	=> SLAVE);
	
		log_indentation_down;
	end movable_test;


	
	
	
	
	
	
	
	procedure drag_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level) 
	is
		use et_board_ops_ratsnest;
		use et_modes.schematic;
		use et_undo_redo;
		use et_commit;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			netchanger_cursor : pac_netchangers.cursor;
			
			-- The sheet where the netchanger is located
			-- in the schematic:
			sheet : type_sheet;
			
			location : type_object_position;
			ports_old : type_netchanger_ports;
			ports_new : type_netchanger_ports;

			
			procedure move (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) 
			is begin
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

			
				set_position (netchanger, to_netchanger_position (location));
			end move;


			use pac_netchangers;
			
		begin
			-- Locate given netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);

			
			-- Get the sheet number where the netchanger is:
			sheet := get_sheet (netchanger_cursor);

			-- log sheet number:
			log (text => "found the netchanger on sheet " & to_string (sheet),
					level => log_threshold + 1);

			
			-- Before the actual drag, the coordinates of the
			-- netchanger ports must be fetched. These coordinates 
			-- will later assist
			-- in changing the positions of connected net segments.
			ports_old := get_netchanger_ports (netchanger_cursor);

			-- Fetch the netchanger position BEFORE the move.
			location := to_object_position (
				get_position_schematic (netchanger_cursor));

			-- Test whether the ports at the current position can be dragged:
			movable_test (module_cursor, index, location, 
				ports_old, log_threshold + 2);
			

			-- Move the netchanger to the new position:
			update_element (
				container	=> module.netchangers,
				position	=> netchanger_cursor,
				process		=> move'access);

			-- Get the NEW absolute positions of the netchanger ports AFTER
			-- the move operation according to location and rotation in schematic.
			ports_new := get_netchanger_ports (netchanger_cursor);

			-- Drag connected net segments:
			drag_net_segments (
				module_cursor	=> module_cursor,
				ports_before	=> ports_old,
				ports_after		=> ports_new,
				sheet			=> get_sheet (location),
				log_threshold	=> log_threshold + 1);

				
			-- The drag operation might result in new port-to-net connections.
			-- So we must insert new ports in segments.
			-- Insert possible new netchanger ports in the nets:
			log_indentation_up;
			
			-- Inserts the netchanger ports in the net segments.
			insert_ports (
				module_cursor	=> module_cursor,
				index			=> index,
				ports			=> ports_new,
				sheet			=> get_sheet (location),
				log_threshold	=> log_threshold + 1);

			log_indentation_down;
		end query_module;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor) 
				& " drag netchanger" & to_string (index) 
				& " to" & to_string (point), 
				level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
				& " drag netchanger" & to_string (index) 
				& " by" & to_string (point),
				level => log_threshold);
		end case;
		

		log_indentation_up;

		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);
		end if;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);
		end if;
			
			
		update_ratsnest (module_cursor, log_threshold + 1);
			
		log_indentation_down;
	end drag_netchanger;



	

	




	
	

	procedure move_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id;
		coordinates		: in type_coordinates;
		sheet			: in type_sheet_relative;
		point			: in type_vector_model;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level) 
	is
		use et_board_ops_ratsnest;
		use et_modes.schematic;
		use et_undo_redo;
		use et_commit;

		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			netchanger_cursor : pac_netchangers.cursor;
			
			-- The sheet where the netchanger is located
			-- before and after the move operation:
			sheet_old : type_sheet;			
			sheet_new : type_sheet;
			
			ports : type_netchanger_ports;

			
			procedure move (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) 
			is 
				location : type_object_position;			
			begin
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
						location := to_object_position (
							get_position_schematic (netchanger_cursor));
						
						move (
							position	=> location,
							offset		=> to_position_relative (point, sheet));

				end case;

				-- Assign the new position to the netchanger candidate:
				set_position (netchanger, to_netchanger_position (location));
				
				-- Get the new sheet number:
				sheet_new := get_sheet (netchanger);
			end move;

			
		begin
			-- Locate given netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);

			
			-- Get the sheet number where the netchanger is:
			sheet_old := get_sheet (netchanger_cursor);

			-- log sheet number:
			log (text => "found the netchanger on sheet " & to_string (sheet_old),
					level => log_threshold + 1);

			
			-- Delete the old netchanger ports in connected net
			-- segments on the old sheet as they are BEFORE the move:
			delete_ports (
				module_cursor	=> module_cursor,
				index			=> index,
				sheet			=> sheet_old,					
				log_threshold	=> log_threshold + 2);
			
			-- Move the netchanger:
			update_element (
				container	=> module.netchangers,
				position	=> netchanger_cursor,
				process		=> move'access);
				
			-- Get the NEW absolute positions of the netchanger 
			-- ports AFTER the move operation:
			ports := get_netchanger_ports (netchanger_cursor);
			
			-- Inserts the netchanger ports in the net segments
			-- on the new sheet:
			insert_ports (
				module_cursor	=> module_cursor,
				index			=> index,
				ports			=> ports,
				sheet			=> sheet_new,
				log_threshold	=> log_threshold + 2);

		end query_module;

		
	begin
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_cursor)
					& " move netchanger " & to_string (index) 
					& " to sheet " & to_string (sheet) 
					& to_string (point),
					 level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_cursor)
					& " move netchanger " & to_string (index) 
					& " by " & relative_to_string (sheet) & " sheet(s) " 
					& to_string (point),
					level => log_threshold);
		end case;

		
		log_indentation_up;
		
		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);
		end if;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);

			
		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);
		end if;
			
			
		update_ratsnest (module_cursor, log_threshold + 1);
		
		log_indentation_down;
	end move_netchanger;




	
	
	
	
	procedure move_netchangers (
		module_cursor	: in pac_generic_modules.cursor;
		sheet_old		: in type_sheet;
		offset			: in type_sheet_relative;
		log_threshold	: in type_log_level)
	is 

	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is 
			netchanger_cursor : pac_netchangers.cursor := 
				module.netchangers.first;
	
	
			-- Moves the netchanger candidate by
			-- the given offset if it is on the given sheet_old:
			procedure query_netchanger (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger)
			is begin
				if get_sheet (netchanger) = sheet_old then
					move_netchanger (netchanger, offset);
				end if;
			end;
			
			
		begin
			-- Iterate through the netchangers of the module:
			while has_element (netchanger_cursor) loop
				module.netchangers.update_element (
					netchanger_cursor, query_netchanger'access);
					
				next (netchanger_cursor);
			end loop;
		end query_module;
		
	
	begin	
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);
	
	end move_netchangers;

	
	
	
	
	
	
	
	
	procedure move_netchangers_on_sheet_delete (
		module_cursor	: in pac_generic_modules.cursor;
		sheet_delete	: in type_sheet;	
		log_threshold	: in type_log_level)
	is		
		sheets_total : type_sheet;
		
		-- We start processing the sheets with the
		-- sheet after sheet_delete:
		sheet_start : type_sheet := sheet_delete + 1;
		
		use et_schematic_ops_sheets;

		
		procedure do_it is 
			netchanger_cursor : pac_netchangers.cursor;		
		begin
			-- Process the sheets from sheet_start to
			-- the last sheet:
			for i in sheet_start .. sheets_total loop
				log (text => "sheet " & to_string (i), level => log_threshold + 1);
				log_indentation_up;

				-- Iterate through the netchangers of the module.
				-- Start with the first netchanger:
				netchanger_cursor := get_first_netchanger (module_cursor);
				
				while has_element (netchanger_cursor) loop
					log (text => "netchanger " & get_netchanger_name (netchanger_cursor),
						level => log_threshold + 1);
						
					log_indentation_up;
				
					-- Move the netchangers:
					move_netchangers (
						module_cursor	=> module_cursor,
						sheet_old		=> i, -- the current sheet
						offset			=> - 1, -- one sheet down
						log_threshold	=> log_threshold + 1);
						
					log_indentation_down;
					
					next (netchanger_cursor);
				end loop;
				
				
				log_indentation_down;
			end loop;
		end do_it;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " move all netchangers one sheet downward."
			& " Sheet to be deleted: " & to_string (sheet_delete),
			level => log_threshold);
		
		log_indentation_up;
		
		
		-- Get the total number of sheets:
		sheets_total := get_sheet_count (module_cursor);
	
		
		-- If the sheet to be deleted is not the last sheet
		-- of the module, then proceed further. Otherwise
		-- there is nothing to do:
		if sheet_delete < sheets_total then
			do_it;			
		elsif sheet_delete = sheets_total then
			log (text => "The last sheet was given. Nothing to do.",
				level => log_threshold);
		else
			-- sheet_delete is greater than sheets_total:
			raise constraint_error;
		end if;

		log_indentation_down;
	end move_netchangers_on_sheet_delete;

	
	
	
	
	
	
	
	



	procedure rotate_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id;
		toggle			: in boolean := false;
		rotation		: in type_rotation_0_90;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level) 
	is
		use et_board_ops_ratsnest;
		use et_modes.schematic;
		use et_undo_redo;
		use et_commit;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			netchanger_cursor : pac_netchangers.cursor;

			-- The sheet where the netchanger is located
			-- in the schematic:
			sheet : type_sheet;

			ports : type_netchanger_ports;

			
			procedure rotate (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) 
			is begin
				if toggle then
					toggle_rotation (netchanger);
					-- CS log messages
				else
					set_rotation (netchanger, rotation);
				end if;
			end;
			
			
			use pac_netchangers;
		begin
			-- Locate given netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);


			-- Get the sheet number where the netchanger is:
			sheet := get_sheet (netchanger_cursor);

			-- log sheet number:
			log (text => "found the netchanger on sheet " & to_string (sheet),
					level => log_threshold + 1);

					
			-- Delete the old netchanger ports in connected
			-- net segments as they are BEFORE the rotation:
			delete_ports (
				module_cursor	=> module_cursor,
				index			=> index,
				sheet			=> sheet,
				log_threshold	=> log_threshold + 2);
			
			-- Rotate the netchanger to the new rotation:
			update_element (
				container	=> module.netchangers,
				position	=> netchanger_cursor,
				process		=> rotate'access);

			-- Get the NEW absolute positions of the netchanger
			-- ports AFTER the rotation:
			ports := get_netchanger_ports (netchanger_cursor);

			-- Inserts the new netchanger ports in the net segments:
			insert_ports (
				module_cursor	=> module_cursor,
				index			=> index,
				ports			=> ports,
				sheet			=> sheet,
				log_threshold	=> log_threshold + 2);

		end query_module;
		
		
	begin
		if toggle then
			log (text => "module " & to_string (module_cursor)
				& " toggle rotation of netchanger " & to_string (index),
				level => log_threshold);
		
		else
			log (text => "module " & to_string (module_cursor)
				& " rotate netchanger " & to_string (index) 
				& " to " & to_string (rotation), 
				level => log_threshold);
		end if;
		
		log_indentation_up;

		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);
		end if;
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		
		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);
		end if;
		
		
		update_ratsnest (module_cursor, log_threshold + 1);
		
		log_indentation_down;
	end rotate_netchanger;

	

	

	

	

	
	procedure copy_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		destination		: in type_netchanger_position_schematic;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is

		use et_modes.schematic;
		use et_undo_redo;
		use et_commit;

	
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			netchanger_cursor : pac_netchangers.cursor;

			ports : type_netchanger_ports;

			
			index_new : type_netchanger_id;
			netchanger : type_netchanger; -- the copy
			rotation : type_rotation_0_90;
			inserted : boolean;
			
			use pac_netchangers;
		begin
			-- Locate given original netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);

			
			-- Get the next available index to be used 
			-- for the new netchanger:
			index_new := get_next_netchanger_index (module_cursor);
		
			log (text => "netchanger index is " & to_string (index_new),
				level => log_threshold + 1);

			-- Take a copy of the original netchanger.
			-- (This operation copies the direction also.)
			netchanger := element (netchanger_cursor);

			-- Backup the rotation of the original netchanger:
			rotation := get_rotation (netchanger);
			
			-- Set the position of the copy as requested
			-- by the caller:
			set_position (netchanger, destination);

			-- log (text => "new netchanger position: "
			-- 	 & to_string (destination),
			-- 	 level => log_threshold + 1);
			
			-- Since no valid rotation was provided 
			-- by the caller (default 0 degrees),
			-- assign the rotation of the original to
			-- the copy:
			set_rotation (netchanger, rotation);
			
			-- Insert the new netchanger in the module:
			insert (
				container 	=> module.netchangers,
				key			=> index_new,
				new_item	=> netchanger,
				position	=> netchanger_cursor,
				-- points now to the new netchanger
				inserted	=> inserted); 
				-- flag "inserted" not further evaluated. 
				-- should always be true

			-- Get the absolute positions of the 
			-- new netchanger ports according to 
			-- location and rotation in schematic:
			ports := get_netchanger_ports (netchanger_cursor);

			-- Inserts the new netchanger ports in the 
			-- net segments:
			insert_ports (
				module_cursor	=> module_cursor,
				index			=> index_new,
				ports			=> ports,
				sheet			=> get_sheet (destination),
				log_threshold	=> log_threshold + 1);

		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			 & " copy netchanger " & to_string (index)
			 & " to " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);
		end if;
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		
		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);
		end if;
		
		log_indentation_down;		
	end copy_netchanger;







	
	

	procedure rename_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index_old		: in type_netchanger_id; -- 1
		index_new		: in type_netchanger_id; -- 14
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is
	
		use et_modes.schematic;
		use et_undo_redo;
		use et_commit;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			netchanger_cursor : pac_netchangers.cursor;

			sheet : type_sheet;
			ports : type_netchanger_ports;

			netchanger : type_netchanger; -- the copy
			inserted : boolean;
			
			use pac_netchangers;
		begin
			-- The renaming is basically two steps:
			-- 1. deleting the netchanger with the old index
			-- 2. adding a copy of the old netchanger with 
			--    the new index.
			
			-- Locate given original netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index_old);

			
			-- If a netchanger with the index_new already exists
			-- in the module, then no renaming will be done:
			if netchanger_exists (module_cursor, index_new) then

				log (WARNING, " Netchanger " & to_string (index_new)
						& " already exists !");
				
			else
				-- Netchanger with new index does not exist, so
				-- the renaming operation is allowed:
				
				-- Step 1:
				
				-- Take a copy of the original netchanger:
				netchanger := element (netchanger_cursor);
				sheet := get_sheet (netchanger_cursor);

				-- Delete the old netchanger completely:
				delete_netchanger (module_cursor, 
					index_old, NO_COMMIT, log_threshold + 1);


				-- Step 2:					
				
				-- Insert the new netchanger in the module:
				insert (
					container 	=> module.netchangers,
					key			=> index_new,
					new_item	=> netchanger,
					position	=> netchanger_cursor,
					-- points now to the new netchanger
					inserted	=> inserted); 
					-- flag "inserted" not further evaluated. 
					-- should always be true

				-- Get the absolute positions of the 
				-- new netchanger ports according to 
				-- location and rotation in schematic:
				ports := get_netchanger_ports (netchanger_cursor);

				-- Inserts the new netchanger ports in the 
				-- net segments:
				insert_ports (
					module_cursor	=> module_cursor,
					index			=> index_new,
					ports			=> ports,
					sheet			=> sheet,
					log_threshold	=> log_threshold + 1);
			end if;

		end query_module;


	begin
		log (text => "module " & to_string (module_cursor) 
			 & " rename netchanger " & to_string (index_old)
			 & " to " & to_string (index_new),
			level => log_threshold);

		log_indentation_up;

		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);
		end if;
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		
		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);
		end if;
		
		log_indentation_down;		
	end rename_netchanger;
		


	
	
	


	procedure delete_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level) 
	is
		use et_board_ops_ratsnest;
		use et_modes.schematic;
		use et_undo_redo;
		use et_commit;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			netchanger_cursor : pac_netchangers.cursor;

			-- The sheet where the netchanger is located
			-- in the schematic:
			sheet : type_sheet;

			use pac_netchangers;
		begin
			-- Locate given netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);


			-- Get the sheet number where the netchanger is:
			sheet := get_sheet (netchanger_cursor);

			-- log sheet number:
			log (text => "found the netchanger on sheet " & to_string (sheet),
					level => log_threshold + 1);


			-- Delete netchanger ports in nets:
			delete_ports (
				module_cursor	=> module_cursor,
				index			=> index,
				sheet			=> sheet,					
				log_threshold	=> log_threshold + 2);

			-- Delete the netchanger itself:
			delete (module.netchangers, netchanger_cursor);

		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " delete netchanger " & to_string (index),
			level => log_threshold);

		log_indentation_up;

		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);
		end if;
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		
		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);
		end if;

		
		update_ratsnest (module_cursor, log_threshold + 1);
					
		log_indentation_down;		
	end delete_netchanger;

	

	
	




	procedure delete_netchangers (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		log_threshold	: in type_log_level)

	is
		use pac_netchanger_ids;
		netchanger_indexes : pac_netchanger_ids.list;


		-- This procedure queries a netchanger by its index:
		procedure query_index (c : pac_netchanger_ids.cursor) is
			-- The index candidate being queried:
			index : type_netchanger_id renames element (c);
			position : type_object_position;
		begin
			-- Get the position of the netchanger:
			position := get_netchanger_position (module_cursor, index);

			-- If the netchanger is on the giben sheet, then
			-- delete it:
			if get_sheet (position) = sheet then
				delete_netchanger (module_cursor, index, NO_COMMIT,
					log_threshold + 1);
			end if;
		end query_index;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " delete all netchangers on sheet " & to_string (sheet),
			level => log_threshold);

		log_indentation_up;

		-- Get the indexes of all netchangers in the module:
		netchanger_indexes := get_netchangers (module_cursor);

		-- Iterate through the indexes:
		netchanger_indexes.iterate (query_index'access);
		
		log_indentation_down;
	end delete_netchangers;

	




	

	
	procedure dissolve_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level) 
	is
		use et_board_ops_ratsnest;
		use et_modes.schematic;
		use et_undo_redo;
		use et_commit;

		-- The names of the nets connected with the
		-- MASTER and the SLAVE side of the netchanger:
		name_M, name_S : pac_net_name.bounded_string;
		
		-- If both nets exist, then the dissolving
		-- is granted:
		dissolving_allowed : boolean := false;
		
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is
			use pac_netchangers;
			netchanger_cursor : pac_netchangers.cursor;

			-- The sheet where the netchanger is located
			-- in the schematic:
			sheet : type_sheet;
						
			use pac_nets;
			net_M, net_S : pac_nets.cursor;
		begin
			-- Locate given netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);

			-- Get the sheet number where the netchanger is:
			sheet := get_sheet (netchanger_cursor);

			-- log sheet number:
			log (text => "found the netchanger on sheet " & to_string (sheet),
					level => log_threshold + 1);

					
			
			-- Query net on MASTER port side:
			net_M := get_connected_net (module_cursor, index, MASTER);
			
			if has_element (net_M) then
				name_M := get_net_name (net_M);
				
				log (text => "MASTER port is connected with net " 
					& to_string (name_M), level => log_threshold + 1);
					
			else
				log (WARNING, "No net on MASTER port found !");
			end if;


			
			-- Query net on SLAVE port side:
			net_S := get_connected_net (module_cursor, index, SLAVE);
			
			if has_element (net_S) then
				name_S := get_net_name (net_S);
				
				log (text => "SLAVE port is connected with net " 
					& to_string (name_s), level => log_threshold + 1);

			else
				log (WARNING, "No net on SLAVE port found !");
			end if;


			-- Set the flag dissolving_allowed if a
			-- net on the MASTER and a net on the SLAVE side
			-- has been found:
			if has_element (net_M) and has_element (net_S) then
				dissolving_allowed := true;
			end if;
		end query_module;

		
		
		-- This procedure renames the net on the SLAVE
		-- side by the net name on the MASTER side
		-- and deletes the targeted netchanger:
		procedure do_dissolve is
			use et_schematic_ops_nets;
		begin
			rename_net (
				module_cursor	=> module_cursor,
				net_name_before	=> name_S,
				net_name_after	=> name_M,
				all_sheets		=> true,
				-- CS currently we rename on all
				-- sheets. see et_schematic_ops_nets
				log_threshold	=> log_threshold + 1);

				
			delete_netchanger (
				module_cursor	=> module_cursor,
				index			=> index,
				commit_design	=> NO_COMMIT,
				log_threshold	=> log_threshold + 1);
				
		end do_dissolve;
			
		
		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " dissolve netchanger " & to_string (index),
			level => log_threshold);

		log_indentation_up;

		-- Ask for permission to dissolve the netchanger:
		query_element (module_cursor, query_module'access);
				
				
		if dissolving_allowed then
		
			if commit_design = DO_COMMIT then
				-- Commit the current state of the design:
				commit (PRE, verb, noun, log_threshold + 1);
			end if;
			
			
			do_dissolve;	
			
			
			if commit_design = DO_COMMIT then
				-- Commit the new state of the design:
				commit (POST, verb, noun, log_threshold + 1);
			end if;
		
		
			update_ratsnest (module_cursor, log_threshold + 1);
		end if;
				
		log_indentation_down;		
	end dissolve_netchanger;

	
	
	
	
	
	


	procedure show_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			netchanger_cursor : pac_netchangers.cursor;

			-- The sheet where the netchanger is located
			-- in the schematic:
			sheet : type_sheet;

			use pac_netchangers;
			
			
			procedure query_netchanger (
				index	: in type_netchanger_id;
				netchanger	: in out type_netchanger)
			is begin
				set_selected (netchanger);
			end query_netchanger;
			
			
		begin
			-- Locate given netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);


			-- Get the sheet number where the netchanger is:
			sheet := get_sheet (netchanger_cursor);

			-- log sheet number:
			log (text => "found the netchanger on sheet " & to_string (sheet),
					level => log_threshold + 1);

			-- Set the netchanger as selected both
			-- in schematic and board drawing:
			module.netchangers.update_element (
				netchanger_cursor, query_netchanger'access);

		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " show netchanger " & to_string (index),
			level => log_threshold);

		log_indentation_up;

		-- Deselect all objects of previous show operations
		-- so that nothing is highlighted anymore:
		et_schematic_ops_groups.reset_objects (module_cursor,
			log_threshold + 1);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		log_indentation_down;		
	end show_netchanger;


	
	
	
	
	
	
	
	procedure set_netchanger_direction (
		module_cursor	: in pac_generic_modules.cursor;
		index			: in type_netchanger_id; -- 1,2,3,...
		toggle			: in boolean := false;
		direction		: in type_netchanger_direction;
		commit_design	: in type_commit_design := DO_COMMIT;
		log_threshold	: in type_log_level)
	is
	
		use et_modes.schematic;
		use et_undo_redo;
		use et_commit;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			netchanger_cursor : pac_netchangers.cursor;

			-- The sheet where the netchanger is located
			-- in the schematic:
			sheet : type_sheet;

			ports : type_netchanger_ports;
						
			
			procedure set_direction (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) 
			is begin
				if toggle then
					toggle_direction (netchanger);				
				else
					set_direction (netchanger, direction);
				end if;

				-- CS: log message
			end;

			
			
			use pac_netchangers;
		begin
			-- Locate given netchanger in the module:
			netchanger_cursor := get_netchanger (module_cursor, index);


			-- Get the sheet number where the netchanger is:
			sheet := get_sheet (netchanger_cursor);

			-- log sheet number:
			log (text => "found the netchanger on sheet " & to_string (sheet),
					level => log_threshold + 1);


			-- Delete the old netchanger ports in connected
			-- net segments as they are BEFORE the direction change:
			delete_ports (
				module_cursor	=> module_cursor,
				index			=> index,
				sheet			=> sheet,
				log_threshold	=> log_threshold + 2);
			

			update_element (
				container	=> module.netchangers,
				position	=> netchanger_cursor,
				process		=> set_direction'access);

			
			-- Get the NEW absolute positions of the netchanger
			-- ports AFTER the direction change:
			ports := get_netchanger_ports (netchanger_cursor);

			-- Inserts the new netchanger ports in the net segments:
			insert_ports (
				module_cursor	=> module_cursor,
				index			=> index,
				ports			=> ports,
				sheet			=> sheet,
				log_threshold	=> log_threshold + 2);

		end query_module;

		
	begin
		if toggle then
			log (text => "module " & to_string (module_cursor) 
				& " toggle direction of netchanger " & to_string (index),
				level => log_threshold);

		else
			log (text => "module " & to_string (module_cursor) 
				& " set netchanger " & to_string (index)
				& " direction " & to_string (direction),
				level => log_threshold);
		end if;
		
		log_indentation_up;

		
		if commit_design = DO_COMMIT then
			-- Commit the current state of the design:
			commit (PRE, verb, noun, log_threshold + 1);
		end if;
		
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_module'access);
		
		
		if commit_design = DO_COMMIT then
			-- Commit the new state of the design:
			commit (POST, verb, noun, log_threshold + 1);
		end if;
		
		log_indentation_down;		
	end set_netchanger_direction;

	
	
	
	
	
	
	function get_object_name (
		object : in type_object_netchanger)
		return string
	is begin
		return get_netchanger_name (object.netchanger_cursor);
	end;

	
	
	function get_object_id (
		object : in type_object_netchanger)
		return type_netchanger_id
	is begin
		return get_netchanger_id (object.netchanger_cursor);
	end;

	

	function get_rotation (
		object : in type_object_netchanger)
		return type_rotation_0_90
	is begin
		return get_rotation (object.netchanger_cursor);
	end;
	
	
	function get_direction (
		object : in type_object_netchanger)
		return type_netchanger_direction
	is begin
		return get_direction (object.netchanger_cursor);
	end;

	
	
	procedure modify_status (
		module_cursor	: in pac_generic_modules.cursor;
		netchanger		: in type_object_netchanger;
		operation		: in type_status_operation;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is	
			
			procedure query_netchanger (
				name	: in type_netchanger_id;
				nc		: in out type_netchanger)
			is begin
				modify_status (nc, operation);
				-- log (text => "done", level => log_threshold + 1);

				-- If the netchanger is set as moving, then
				-- backup the original position:
				if get_action (operation) = SET and
					get_flag (operation) = MOVING then

					object_original_position := get_place (get_position (nc));
				end if;
			end query_netchanger;

			
		begin
			module.netchangers.update_element (
				netchanger.netchanger_cursor, query_netchanger'access);
				
		end query_module;
		

	begin
		log (text => "module " & to_string (module_cursor)
			& " modify status of netchanger "
			& get_object_name (netchanger)
			& " / " & to_string (operation),
			level => log_threshold);


		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end modify_status;





	
	
	
	procedure propose_netchangers (
		module_cursor	: in pac_generic_modules.cursor;
		catch_zone		: in type_catch_zone;
		count			: in out natural;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_netchangers;
			netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;

			
			procedure query_netchanger (
				name		: in type_netchanger_id;
				netchanger	: in out type_netchanger)
			is begin
				if in_catch_zone (netchanger, catch_zone, active_sheet) then
					log (text => to_string (name), level => log_threshold + 1);
					
					set_proposed (netchanger);
					count := count + 1;
				end if;
			end query_netchanger;

			
		begin
			-- Iterate through the netchangers:
			while has_element (netchanger_cursor) loop
				module.netchangers.update_element (
					netchanger_cursor, query_netchanger'access);
					
				next (netchanger_cursor);
			end loop;
		end query_module;


		
	begin
		log (text => "module " & to_string (module_cursor)
			& " propose netchangers in " & to_string (catch_zone),
			level => log_threshold);
		
		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,		   
			process		=> query_module'access);

		log_indentation_down;
	end propose_netchangers;

	
	
	
	
	
	
	
	
	
	procedure reset_status_netchangers (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
		
			use pac_netchangers;
			netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;

			
			procedure query_netchanger (
				name		: in type_netchanger_id;
				netchanger	: in out type_netchanger)
			is begin
				log (text => to_string (name), level => log_threshold + 1);
				reset_status (netchanger);
			end query_netchanger;

			
		begin
			-- Iterate through the netchangers:
			while has_element (netchanger_cursor) loop
				module.netchangers.update_element (
					netchanger_cursor, query_netchanger'access);
					
				next (netchanger_cursor);
			end loop;
		end query_module;
		
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " reset status of all netchangers", 
			level => log_threshold);

		log_indentation_up;
		
		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end reset_status_netchangers;

	
	
	
	
	

	
	
	
	
	function get_first_netchanger (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;
		log_threshold	: in type_log_level)
		return type_object_netchanger
	is
		result : type_object_netchanger;

		
		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_netchangers;
			netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;

			proceed : boolean := true;

			
			procedure query_netchanger (
				name		: in type_netchanger_id;
				netchanger	: in type_netchanger)
			is 
			
				procedure set_result is begin
					log (text => " found " & to_string (name), level => log_threshold + 2);
					result.netchanger_cursor := netchanger_cursor;
					proceed := false; -- no further probing required
				end set_result;

			
			begin
				log (text => to_string (name), level => log_threshold + 1);
				case flag is
					when PROPOSED =>
						if is_proposed (netchanger) then
							set_result;
						end if;
	
					when SELECTED =>
						if is_selected (netchanger) then
							set_result;
						end if;
	
					when others => null; -- CS
				end case;
			end query_netchanger;

	
		begin
			-- Iterate through the netchangers:
			while has_element (netchanger_cursor) and proceed loop
				query_element (netchanger_cursor, query_netchanger'access);
				next (netchanger_cursor);
			end loop;

			if proceed then
				log (text => "nothing found", level => log_threshold);
			end if;
		end query_module;
		
		
	
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first netchanger / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;

		return result;
	end get_first_netchanger;

	
	
	
	
------------------------------------------------------------------------------------------

-- OBJECTS:
	

	function get_count (
		objects : in pac_objects.list)
		return natural
	is begin
		return natural (objects.length);
	end get_count;
	

	
	
	
	function get_first_object (
		module_cursor	: in pac_generic_modules.cursor;
		flag			: in type_flag;								 
		log_threshold	: in type_log_level)
		return type_object
	is
		result_category 	: type_object_category := CAT_VOID;
		result_netchanger	: type_object_netchanger;

		use pac_netchangers;
	begin
		log (text => "module " & to_string (module_cursor)
			& " looking up the first object / " & to_string (flag),
			level => log_threshold);

		log_indentation_up;

		
		-- SEARCH FOR A NETCHANGER:
		
		-- If a netchanger has been found, then go to the end of this procedure:
		result_netchanger := get_first_netchanger (module_cursor, flag, log_threshold + 1);

		if has_element (result_netchanger.netchanger_cursor) then
			-- A netchanger has been found.
			log (text => get_object_name (result_netchanger),
				 level => log_threshold + 1);
			
			result_category := CAT_NETCHANGER;
		end if;
		
		-- If nothing has been found then the category is CAT_VOID.
		if result_category /= CAT_VOID then
			goto end_of_search;
		end if;




	<<end_of_search>>
		
		-- If nothing has been found then the category is CAT_VOID.
		log_indentation_down;

		
		
		case result_category is
			when CAT_VOID =>
				return (cat => CAT_VOID);

			when CAT_NETCHANGER =>
				return (CAT_NETCHANGER, result_netchanger);
				
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

			-- This procedure queries the netchangers
			-- and collects those which have the given flag set:			
			procedure query_netchangers is
				use pac_netchangers;
				netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;
				

				-- Queries a unit for its status flag
				-- and appends it to the result:
				procedure query_netchanger (
					name		: in type_netchanger_id;
					netchanger	: in type_netchanger) 
				is 

					-- This procedure appends the matching
					-- netchanger to the result:
					procedure collect is begin
						log (text => to_string (name), level => log_threshold + 4);
						
						result.append ((
							cat			=> CAT_NETCHANGER,
							netchanger	=> (netchanger_cursor => netchanger_cursor)));

					end collect;
						
					
				begin
					log (text => to_string (name), level => log_threshold + 2);
					log_indentation_up;
					
					case flag is
						when PROPOSED =>
							if is_proposed (netchanger) then
								collect;
							end if;

						when SELECTED =>
							if is_selected (netchanger) then
								collect;
							end if;

						when others => null; -- CS
					end case;					

					log_indentation_down;
				end query_netchanger;

				
			begin
				log (text => "query_netchangers", level => log_threshold + 1);
				log_indentation_up;
				
				-- Iterate the netchangers of the module:
				while has_element (netchanger_cursor) loop
					query_element (netchanger_cursor, query_netchanger'access);
					next (netchanger_cursor);
				end loop;

				log_indentation_down;
			end query_netchangers;
			
			
		begin
			query_netchangers;
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
			& " modify status of object "
			& type_object_category'image (object.cat)
			& " / " & to_string (operation),
			level => log_threshold);

		log_indentation_up;
		
		case object.cat is
			when CAT_NETCHANGER =>
				modify_status (module_cursor, object.netchanger,
					operation, log_threshold + 1);
			
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

	
	
	
	
	procedure reset_status_objects (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is 

		procedure reset_devices is begin
			-- Reset netchangers:
			reset_status_netchangers (module_cursor, log_threshold + 1);
			-- CS notes, properties, ...
		end;

		
	begin
		log (text => "module " & to_string (module_cursor) 
			& " reset objects",
			level => log_threshold);

		log_indentation_up;
		reset_devices;		
		log_indentation_down;
	end reset_status_objects;

	
	
	
	
	
	
	
	
	procedure move_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " move object " 
			-- CS & to_string (object)
			& " to " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NETCHANGER =>

				move_netchanger (
					module_cursor	=> module_cursor,
					index			=> get_object_id (object.netchanger),
					coordinates		=> absolute,
					sheet			=> active_sheet,
					point			=> destination,
					log_threshold	=> log_threshold + 1);
				

			--when CAT_VOID =>
			when others =>
				null;
		end case;		
		
		log_indentation_down;
	end move_object;


	
	
	
	
	

	procedure rotate_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " rotate object " 
			-- CS & to_string (object)
			& " by 90 degrees",
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NETCHANGER =>
				rotate_netchanger (
					module_cursor	=> module_cursor,
					index			=> get_object_id (object.netchanger),
					toggle			=> true,
					rotation		=> 0.0, -- don't care, see specs
					log_threshold	=> log_threshold + 1);
			

			when others =>
				null;
		end case;		
		
		log_indentation_down;
	end rotate_object;

	
	



	

	procedure set_segments_moving (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_netchangers;
			netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;

			
			procedure query_netchanger (
				index		: in type_netchanger_id;
				netchanger	: in type_netchanger)
			is 

				-- Get the sheet where the candidate netchanger is:
				sheet : type_sheet := get_sheet (netchanger);

				
				-- This procedure takes a port position
				-- and sets start or end points of net segments which are
				-- at the port position as "moving":
				procedure query_position (place : in type_vector_model) is
					position : type_object_position;
				begin
					-- Compose the position of inquiry 
					-- from port position and sheet number:
					position := to_position (place, sheet);

					-- Set the connected net segments as "moving":
					et_schematic_ops_nets.set_segments_moving (
						module_cursor, position, log_threshold + 2);
				end query_position;

					
				-- These are the ports of the
				-- candidate netchanger:
				port_positions : type_netchanger_ports;

										 
			begin
				if is_selected (netchanger) then
					log (text => "netchanger " & get_netchanger_name (index),
						level => log_threshold + 1);

					-- Get the port positions of the candidate netchanger:
					port_positions := get_netchanger_ports (netchanger);

					-- Iterate the port positions (a netchanger has only two):
					query_position (port_positions.master);
					query_position (port_positions.slave);
				end if;
				
				log_indentation_down;
			end query_netchanger;
			
			
		begin
			-- Iterate through the netchangers:
			while has_element (netchanger_cursor) loop
				query_element (netchanger_cursor, query_netchanger'access);
				next (netchanger_cursor);
			end loop;
		end query_module;

		
	begin
		log (text => "module " & to_string (module_cursor)
			& " set net segments (connected with selected netchangers) moving.",
			level => log_threshold);

		log_indentation_up;

		generic_modules.update_element (
			position	=> module_cursor,
			process		=> query_module'access);

		log_indentation_down;
	end set_segments_moving;

	


	
	
	
	procedure drag_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_vector_model;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " drag object " 
			-- CS & to_string (object)
			& " to " & to_string (destination),
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NETCHANGER =>

				drag_netchanger (
					module_cursor	=> module_cursor,
					index			=> get_object_id (object.netchanger),
					coordinates		=> absolute,
					point			=> destination,
					log_threshold	=> log_threshold + 1);
				
				
			when others =>
				null;
		end case;		
		
		log_indentation_down;
	end drag_object;

	
	
	
	

	procedure dissolve_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " dissolve object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NETCHANGER =>

				dissolve_netchanger (
					module_cursor	=> module_cursor,
					index			=> get_object_id (object.netchanger),
					log_threshold	=> log_threshold + 1);

				
			when others =>
				null;
		end case;		
		
		log_indentation_down;
	end dissolve_object;

	
	
	
	
	
	

	procedure delete_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " delete object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NETCHANGER =>

				delete_netchanger (
					module_cursor	=> module_cursor,
					index			=> get_object_id (object.netchanger),
					log_threshold	=> log_threshold + 1);

				
			when others =>
				null;
		end case;		
		
		log_indentation_down;
	end delete_object;









	procedure set_object_direction (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " set object direction",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NETCHANGER =>

				set_netchanger_direction (
					module_cursor	=> module_cursor,
					index			=> get_object_id (object.netchanger),
					toggle			=> true,
					direction		=> FORWARD, -- don't care. see specs
					log_threshold	=> log_threshold + 1);

				
			when others =>
				null;
		end case;		
		
		log_indentation_down;
	end set_object_direction;




	
	
	
	
	
	procedure show_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		log_threshold	: in type_log_level)
	is 
		error : boolean := false;
	begin
		log (text => "module " & to_string (module_cursor)
			& " show object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NETCHANGER =>

				show_netchanger (
					module_cursor	=> module_cursor,
					index			=> get_object_id (object.netchanger),
					log_threshold	=> log_threshold + 1);

						
			when others =>
				null;
		end case;		
		
		log_indentation_down;
	end show_object;


	
	
	
	

	procedure rename_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		new_name		: in type_netchanger_id;
		log_threshold	: in type_log_level)
	is begin
		log (text => "module " & to_string (module_cursor)
			& " rename object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NETCHANGER =>
				
				rename_netchanger (
					module_cursor	=> module_cursor,
					index_old		=> get_object_id (object.netchanger),
					index_new		=> new_name,
					log_threshold	=> log_threshold + 1);
				
			when others => null;
		end case;		
		
		log_indentation_down;
	end rename_object;


	
	
	


	procedure copy_object (
		module_cursor	: in pac_generic_modules.cursor;
		object			: in type_object;
		destination		: in type_position; -- x/y/rotation
		log_threshold	: in type_log_level)
	is 
		object_position : type_netchanger_position_schematic;
	begin
		log (text => "module " & to_string (module_cursor)
			& " copy object",
			-- CS & to_string (object)
			level => log_threshold);

		log_indentation_up;

		case object.cat is
			when CAT_NETCHANGER =>

				-- Build the full position of
				-- the new netchanger.
				-- The copy operation takes place on the
				-- active sheet only:
				object_position := to_netchanger_position (
					sheet => active_sheet, 
					place => get_place (destination), 
					rotation => get_rotation (destination));

				-- Do the final copy operation:
				copy_netchanger (
					module_cursor	=> module_cursor,
					index			=> get_object_id (object.netchanger),					
					destination		=> object_position,
					log_threshold	=> log_threshold + 1);
								
			when others => null;
		end case;		
		
		log_indentation_down;
	end copy_object;


	
	
end et_schematic_ops_netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
