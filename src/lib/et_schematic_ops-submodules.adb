------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--          SCHEMATIC OPERATIONS ON NETCHANGERS AND SUBMODULES              --
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

with ada.directories;
with et_directory_and_file_ops;

with et_mirroring;					use et_mirroring;
with et_board_coordinates;
with et_generic_stacks;
with et_device_appearance;
with et_package_names;
with et_net_strands;				use et_net_strands;
with et_net_ports;
with et_module_ops;
with et_schematic_ops.units;		use et_schematic_ops.units;
with et_schematic_ops.nets;


package body et_schematic_ops.submodules is


	procedure submodule_not_found (
		name : in pac_module_instance_name.bounded_string) 
	is begin
		log (ERROR, "submodule instance " & enclose_in_quotes (to_string (name)) &
			 " not found !", console => true);
		raise constraint_error;
	end;


	
	procedure netchanger_not_found (
		index : in et_submodules.type_netchanger_id) 
	is begin
		log (ERROR, "netchanger" & et_submodules.to_string (index) & " not found !", console => true);
		raise constraint_error;
	end;

	

	procedure port_not_at_edge (
		name : in pac_net_name.bounded_string) 
	is 
		use et_string_processing;
	begin
		log (ERROR, "port " & enclose_in_quotes (to_string (name)) &
			" must be at the edge of the submodule !", console => true);
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





	function netchanger_as_port_available (
		module		: in pac_generic_modules.cursor;
		net			: in et_nets.pac_nets.cursor;
		direction	: in et_submodules.type_netchanger_port_name) -- master/slave 
		return boolean 
	is
		
		result : boolean := false; -- to be returned. goes true on the first
		-- suitable netchanger found.

		use et_nets;

		
		procedure query_net (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is
			use pac_strands;
			strand_cursor : pac_strands.cursor := net.strands.first;

			
			procedure query_strand (strand : in type_strand) is
				use pac_net_segments;
				segment_cursor : pac_net_segments.cursor := strand.segments.first;

				
				procedure query_segment (segment : in type_net_segment) is 
					use et_submodules;

					use et_netlists;
					use pac_netchanger_ports;
					port_cursor : pac_netchanger_ports.cursor;

					
					procedure iterate_ports is begin
						while has_element (port_cursor) loop

							-- If the given direction is MASTER, then we must look for a SLAVE netchanger
							-- port (and vice versa) in the net segment.
							if element (port_cursor).port = opposide_port (direction) then 

								-- The opposide port must be not connected. In that case 
								-- suitable netchanger has been found:
								if not port_connected (
									module	=> module,
									port	=> (index	=> element (port_cursor).index,
												port	=> direction)) then
									
									result := true;
									exit; -- no more searching for netchanger ports required
								end if;

							end if;
							
							next (port_cursor);
						end loop;
					end iterate_ports;

					
				begin
					port_cursor := segment.ports.A.netchangers.first;
					iterate_ports;
					
					if not result then
						port_cursor := segment.ports.B.netchangers.first;
						iterate_ports;
					end if;						
				end query_segment;

				
			begin
				while result = false and segment_cursor /= pac_net_segments.no_element loop
					
					query_element (
						position	=> segment_cursor,
						process		=> query_segment'access);
					
					next (segment_cursor);
				end loop;
			end query_strand;

			
		begin
			while result = false and strand_cursor /= pac_strands.no_element loop

				query_element (
					position	=> strand_cursor,
					process		=> query_strand'access);
				
				next (strand_cursor);
			end loop;
		end query_net;

		
	begin
		pac_nets.query_element (
			position	=> net,
			process		=> query_net'access);
		
		return result;
	end netchanger_as_port_available;

	

	

	function submodule_port_exists (
		module			: in et_submodules.pac_submodules.cursor;
		port			: in pac_net_name.bounded_string; -- clock_output
		direction		: in et_submodules.type_netchanger_port_name) -- master/slave
		return boolean 
	is
		result : boolean := false; -- to be returned
		
		use et_string_processing;
		use et_submodules;

		
		submodule_file : pac_submodule_path.bounded_string; -- $ET_TEMPLATES/motor_driver.mod
		module_name : pac_module_name.bounded_string; 
		module_cursor : pac_generic_modules.cursor;

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_nets;
			net_cursor : pac_nets.cursor;

			-- The port being inquired is a net inside the submodule.
			net : constant string := to_string (port);
			use pac_nets;
		begin
			-- locate the net in the submodule
			net_cursor := find (module.nets, to_net_name (net));

			-- If net found, test its scope. If it is global,
			-- then all requirements are met -> result true.
			-- If net is local, then a netchanger is required.
			if net_cursor /= pac_nets.no_element then -- net found

				case element (net_cursor).scope is
					when et_netlists.GLOBAL => 
						result := true;

					when et_netlists.LOCAL =>
						if netchanger_as_port_available (module_cursor, net_cursor, direction) then
							result := true;
						else
							result := false;
						end if;
				end case;
				
			else -- net not found: result is false
				result := false;
			end if;

		end query_nets;

		
	begin
		submodule_file := pac_submodules.element (module).file;

		module_name := to_module_name (remove_extension (to_string (submodule_file)));
		module_cursor := locate_module (module_name);

		pac_generic_modules.query_element (
			position	=> module_cursor,
			process		=> query_nets'access);
		
		return result;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
		
	end submodule_port_exists;




	function submodule_port_exists (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		submod_instance	: in pac_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in pac_net_name.bounded_string) -- RESET
		return boolean 
	is

		use pac_module_instance_name;
		use et_submodules;
		
		result : boolean := false; -- to be returned, goes true once the target has been found

		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			submod_cursor : pac_submodules.cursor;

			procedure query_ports (
			-- Searches the portlist of the submodule for a port having the port_name.
			-- Exits prematurely on match.
				submod_name	: in pac_module_instance_name.bounded_string;
				submodule	: in et_submodules.type_submodule) is
				use pac_net_name;
				use et_submodules.pac_submodule_ports;
				port_cursor : et_submodules.pac_submodule_ports.cursor := submodule.ports.first;
			begin
				while port_cursor /= et_submodules.pac_submodule_ports.no_element loop
					if key (port_cursor) = port_name then
						result := true;
						exit;
					end if;

					next (port_cursor);
				end loop;
			end query_ports;


			use pac_submodules;
			
			
		begin
			if contains (module.submods, submod_instance) then -- submodule found
				submod_cursor := find (module.submods, submod_instance);
				
				query_element (
					position	=> submod_cursor,
					process		=> query_ports'access);
								  
			end if;
		end query_submodules;

		
	begin
		query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);

		return result;
	end submodule_port_exists;
	

	

	
	function get_submodule_port_position (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		submod_name		: in pac_module_instance_name.bounded_string; -- MOT_DRV_3
		port_name		: in pac_net_name.bounded_string; -- RESET
		log_threshold	: in type_log_level)
		return type_object_position
	is
		port_position : type_object_position; -- to be returned		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being inquired

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_submodules;			

			submod_cursor : pac_submodules.cursor;
			submod_position : type_object_position;

			
			procedure query_ports (
				submod_name	: in pac_module_instance_name.bounded_string;
				submodule	: in et_submodules.type_submodule) 
			is
				use et_submodules.pac_submodule_ports;
				port_xy : type_vector_model;
				cursor : et_submodules.pac_submodule_ports.cursor := find (submodule.ports, port_name);
			begin
				if cursor /= et_submodules.pac_submodule_ports.no_element then

					-- If the port exits, get its relative x/y position (relative to submodule position).
					port_xy := element (cursor).position;

					-- Calculate the absolute port position:
					move_by (
						point	=> port_xy,
						offset	=> submod_position.place);

					-- Now port_xy holds the absolute x/y of the port in the schematic.

					-- Assemble the port_position to be returned:
					port_position := to_position (
						point	=> port_xy,
						sheet	=> get_sheet (submod_position)
						);

				else
					log (ERROR, "port " & to_string (port_name) & " not found !",
						 console => true);
				end if;
			end query_ports;


			use pac_submodules;

			
		begin -- query_submodules
			if contains (module.submods, submod_name) then
				submod_cursor := find (module.submods, submod_name); -- the submodule should be there

				log_indentation_up;

				-- get submodule position (sheet/x/y)
				submod_position := element (submod_cursor).position;

				-- look for the given port
				query_element (
					position	=> submod_cursor,
					process		=> query_ports'access);

				log_indentation_down;				
			else
				submodule_not_found (submod_name);
			end if;
		end query_submodules;

		
	begin
		log (text => "module " & to_string (module_name) &
			 " locating submodule " & to_string (submod_name) & 
			 " port " & to_string (port_name) & " ...", level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);
		
		return port_position;
	end get_submodule_port_position;



	

	procedure port_not_provided (port_name : in pac_net_name.bounded_string) is begin
		log (ERROR, "submodule does not provide a port named " &
			 enclose_in_quotes (to_string (port_name)) & " with the desired direction (master/slave) !", console => true);
		raise constraint_error;
	end;


	

	-- Inserts the given submodule port in the net segments.
	-- If the port lands on either the start or end point of a segment, it will
	-- be regarded as "connected" with the segment.
	-- If the port lands between start or end point of a segment, nothing happens
	-- because the docking to net segments is possible on segment ends/starts only.
	-- CS: Automatic splitting the segment into two and placing a junction is not supported
	-- jet and probably not a good idea.
	procedure insert_port (
		module			: in pac_generic_modules.cursor;		-- the module
		instance		: in pac_module_instance_name.bounded_string; -- OSC
		port			: in pac_net_name.bounded_string; -- clock_output
		position		: in type_object_position; -- the port position
		log_threshold	: in type_log_level) 
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			-- This flag goes true on the first match. It signals
			-- all iterations to cancel prematurely.
			port_processed : boolean := false;

			use pac_strands;
			use pac_nets;
			net_cursor : pac_nets.cursor := module.nets.first;

			
			procedure query_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_strand (strand : in out type_strand) is
					use pac_net_segments;
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure change_segment (
						segment : in out type_net_segment) 
					is begin
						-- If port sits on the A or B end of the segment,
						-- then insert it at this end:
						if 	get_A (segment) = get_place (position) then
							insert_submodule_port (segment, A, (instance, port)); -- OSC1, clock_output

							-- signal iterations in upper levels to cancel
							port_processed := true;
						end if;

						
						if get_B (segment) = get_place (position) then
							insert_submodule_port (segment, B, (instance, port)); -- OSC1, clock_output

							-- signal iterations in upper levels to cancel
							port_processed := true;
						end if;
					end change_segment;

					
				begin -- query_strand
					log_indentation_up;

					-- On the first segment, where the port sits on, this loop ends prematurely.
					while not port_processed and segment_cursor /= pac_net_segments.no_element loop
						log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
						
						pac_net_segments.update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> change_segment'access);

						next (segment_cursor);
					end loop;

					log_indentation_down;
				end query_strand;

				
			begin -- query_net
				log_indentation_up;
				
				while not port_processed and strand_cursor /= pac_strands.no_element loop

					-- We pick out only the strands on the targeted sheet:
					if get_sheet (element (strand_cursor).position) = get_sheet (position) then
						log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

						log_indentation_up;
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							level => log_threshold + 1);

						update_element (
							container	=> net.strands,
							position	=> strand_cursor,
							process		=> query_strand'access);
					
						log_indentation_down;
					end if;
						
					next (strand_cursor);
				end loop;
				
				log_indentation_down;
			end query_net;

			
		begin -- query_module
			while not port_processed and net_cursor /= pac_nets.no_element loop
				
				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_net'access);
			
				next (net_cursor);
			end loop;
		end query_module;

		
	begin -- insert_port
		log (text => "inserting submodule port " & enclose_in_quotes (to_string (port)) & " in net at" & 
			 to_string (position => position) & " ...", level => log_threshold);
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_module'access);

		log_indentation_down;

	end insert_port;


	


	
	procedure add_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		port_name		: in pac_net_name.bounded_string; -- clk_out
		position		: in type_vector_model; -- x/y along the edge of the box
		
		direction		: in et_submodules.type_netchanger_port_name; -- master/slave. 
		-- NOTE: has nothing to do with direction of energy flow. It is relevant when 
		-- a netlist is exported. See specification et_submodules.type_submodule_port.
		
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_submodules;

		-- The place where the box is in the parent module:
		submodule_position : type_object_position;

		-- Handling the absolute position of the port requires this variable:
		port_position : type_object_position;
		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_submodules;
			submod_cursor : pac_submodules.cursor;

			procedure query_ports (
				submod_name	: in pac_module_instance_name.bounded_string;
				submodule	: in out et_submodules.type_submodule) 
			is
				use et_submodules.pac_submodule_ports;
				cursor : et_submodules.pac_submodule_ports.cursor;
				inserted : boolean;
				port : et_submodules.type_submodule_port;
			begin
				-- Test whether the submodule provides the given port.
				if submodule_port_exists (
					module		=> submod_cursor,
					port 		=> port_name, -- clock_output
					direction	=> direction -- master/slave
					) then
				
					-- The given port position must be somewhere at the edge
					-- of the submodule. position is relative to the lower left
					-- corner of the box:
					if at_edge (position, submodule.size) then
						port.position := position;
					else
						port_not_at_edge (port_name);
					end if;

					-- set the naming direction of the port:
					port.direction := direction; -- master/slave 
					
					-- Insert the new port in the submodule:
					insert (
						container	=> submodule.ports,
						key			=> port_name,
						new_item	=> port,
						position	=> cursor,
						inserted	=> inserted);

					if not inserted then
						log (ERROR, "port " & 
							enclose_in_quotes (to_string (port_name)) &
							" already in submodule !", console => true);
						raise constraint_error;
					end if;

				else -- port not provided
					port_not_provided (port_name);
				end if;
					
			end query_ports;
			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For inserting the submodule port in the nets
				-- we take a copy of the coordinates of the submodule (the box):
				submodule_position := element (submod_cursor).position;

				log_indentation_up;

 				-- insert the given port in the submodule
				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> query_ports'access);

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;

		
	begin -- add_port
		log (text => "module " & to_string (module_name) &
			" submodule instance " & enclose_in_quotes (to_string (instance)) & 
			" adding port " & enclose_in_quotes (to_string (port_name)) &
			" at" & to_string (position) &
			" direction" & to_string (direction),
			level => log_threshold);

		-- locate parent module
		module_cursor := locate_module (module_name);

		-- add the port to the box in the parent module
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- insert the submodule port in the nets:
		-- We know the absolute position of the box by submodule_position.
		-- We know the position of the port relative to the submodule_position
		-- which is in variable "position".

		-- Build the absoltue port_position:

		-- 1. assume x/y as given by position (which is the relative position):
		set (
			point		=> port_position.place,
			position	=> position); -- the relative port position

		-- 2. move port_position by x/y of submodule_position:
		move_by (
			point		=> port_position.place,
			offset		=> submodule_position.place);

		-- x/y of port_position is now absolute

		-- 3. set sheet number as given by submodule_position:
		set_sheet (
			position	=> port_position,
			sheet		=> get_sheet (submodule_position));

		-- port_position is now ready to insert the submodule port in the nets:
		insert_port (
			module			=> module_cursor,
			instance		=> instance,
			port			=> port_name,
			position		=> port_position,
			log_threshold	=> log_threshold + 1);
		
	end add_port;





	-- Removes a port from the net segments.
	procedure delete_submodule_port (
		module			: in pac_generic_modules.cursor;		-- the module
		port			: in type_submodule_port; -- OSC1 / clock_output
		position		: in type_object_position; -- the submodule position (only sheet matters)
		log_threshold	: in type_log_level) 
	is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
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
						deleted : boolean := false;
					begin
						-- Search for the port and delete it if existing:
						delete_submodule_port (segment, port, deleted);

						if deleted then
							port_processed := true;
						end if;
					end change_segment;

					
				begin -- query_segments
					log_indentation_up;

					-- On the first segment, where the port sits on, this loop ends prematurely.
					while not port_processed and segment_cursor /= pac_net_segments.no_element loop
						log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
						
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
					if get_sheet (element (strand_cursor).position) = get_sheet (position) then
						log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

						log_indentation_up;
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							level => log_threshold + 1);

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

			
		begin -- query_nets
			while not port_processed and net_cursor /= pac_nets.no_element loop
				
				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
			
				next (net_cursor);
			end loop;

			-- CS warning if port_processed still false ?
		end query_nets;

		
	begin -- delete_submodule_port
		log (text => "deleting submodule port in nets ...", level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;

	end delete_submodule_port;



	
	

	procedure submodule_port_not_found (name : in pac_net_name.bounded_string) is begin
		log (ERROR, "port " &
			enclose_in_quotes (to_string (name)) & " not found !", console => true);
		raise constraint_error;
	end;




	procedure delete_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		port_name		: in pac_net_name.bounded_string; -- clk_out
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_submodules;

		-- The place where the box is in the parent module:
		submodule_position : type_object_position;
		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			submod_cursor : pac_submodules.cursor;

			procedure query_ports (
				submod_name	: in pac_module_instance_name.bounded_string;
				submodule	: in out et_submodules.type_submodule) 
			is
				use et_submodules.pac_submodule_ports;
				port_cursor : et_submodules.pac_submodule_ports.cursor;
			begin
				-- Test whether the submodule provides the given port.
				port_cursor := find (submodule.ports, port_name);

				-- If the port is available (at the edge of the box) then
				-- it must be removed from the box.
				if port_cursor /= et_submodules.pac_submodule_ports.no_element then
					delete (submodule.ports, port_cursor);
				else
					submodule_port_not_found (port_name);
				end if;					
			end query_ports;


			use pac_submodules;
			
			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For removing the submodule port from the nets
				-- we take a copy of the coordinates of the submodule (the box):
				submodule_position := element (submod_cursor).position;

				log_indentation_up;

 				-- insert the given port in the submodule
				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> query_ports'access);

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;

		
	begin -- delete_port
		log (text => "module " & to_string (module_name) &
			" submodule instance " & enclose_in_quotes (to_string (instance)) & 
			" deleting port " & enclose_in_quotes (to_string (port_name)),
			level => log_threshold);

		-- locate parent module
		module_cursor := locate_module (module_name);

		-- remove the port from the box in the parent module
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- now the port must be removed from the nets.
		delete_submodule_port (
			module			=> module_cursor,
			port			=> (instance, port_name), -- OSC1 / clock_output
			position		=> submodule_position, -- the submodule position (only sheet matters)
			log_threshold	=> log_threshold + 1);
		
	end delete_port;

	
	
	
	procedure move_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC
		port_name		: in pac_net_name.bounded_string; -- clock_output
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is
		use et_submodules;

		-- The place where the box is in the parent module:
		submodule_position : type_object_position;

		-- Handling the absolute position of the port requires this variable:
		port_position : type_object_position;
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified


		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			submod_cursor : pac_submodules.cursor;

			
			procedure query_ports (
				submod_name	: in pac_module_instance_name.bounded_string;
				submodule	: in out et_submodules.type_submodule) 
			is
				use et_submodules.pac_submodule_ports;
				port_cursor : et_submodules.pac_submodule_ports.cursor;

				
				procedure move (
					port_name	: in pac_net_name.bounded_string;
					port		: in out et_submodules.type_submodule_port) 
				is
					submod_pos_tmp : type_vector_model := submodule_position.place;
					point_tmp : type_vector_model := point;
				begin
					case coordinates is
						when ABSOLUTE =>
							-- From the given point the absolute submodule position must 
							-- be subtracted. This requires inversion of x/y of submodule position.
							-- We accompish that by mirroring along x and y axis.
							mirror_point (submod_pos_tmp, MIRROR_ALONG_X_AXIS);
							mirror_point (submod_pos_tmp, MIRROR_ALONG_Y_AXIS);

							-- Subtract from given point the absolute submodule position:
							move_by (
								point	=> point_tmp,
								offset	=> submod_pos_tmp);

							-- assign the new port position
							port.position := point_tmp;

						when RELATIVE =>
							move_by (
								point	=> port.position,
								offset	=> point);
							
					end case;
					
					-- The port must be somewhere at the edge of the box
					-- of the submodule. The port position is relative to 
					-- the lower left corner of the box:
					if at_edge (port.position, submodule.size) then

						-- Later, for inserting the new port in the nets the
						-- absolute port position must be built:
						port_position := to_position (
									point	=> port.position, -- relative x/y to submodule position
									sheet	=> get_sheet (submodule_position));

						move_by (
							point	=> port_position.place,
							offset	=> submodule_position.place);
						-- now port_position contains the absolute port position

					else
						port_not_at_edge (port_name);
					end if;					
				end move;

				
			begin -- query_ports
				-- Test whether the submodule provides the given port.
				port_cursor := find (submodule.ports, port_name);

				-- If the port is available (at the edge of the box) then
				-- it can be moved:
				if port_cursor /= et_submodules.pac_submodule_ports.no_element then

					update_element (
						container	=> submodule.ports,
						position	=> port_cursor,
						process		=> move'access);
					
				else
					submodule_port_not_found (port_name);
				end if;					
			end query_ports;

			
			use pac_submodules;

			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For moving the submodule port
				-- we take a copy of the coordinates of the submodule (the box):
				submodule_position := element (submod_cursor).position;

				log_indentation_up;

				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> query_ports'access);

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;

		
	begin -- move_port
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving port " & enclose_in_quotes (to_string (port_name)) &
					" to" & to_string (point),
					level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving port " & enclose_in_quotes (to_string (port_name)) &
					" by" & to_string (point),
					level => log_threshold);

		end case;

		-- locate module
		module_cursor := locate_module (module_name);

		-- move the port along the edge of the box:
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- now the old port must be removed from the nets.
		delete_submodule_port (
			module			=> module_cursor,
			port			=> (instance, port_name), -- OSC1 / clock_output
			position		=> submodule_position, -- the submodule position (only sheet matters)
			log_threshold	=> log_threshold + 1);

		-- Now, port_position contains the new absolute port position in the schematic.
		-- So we insert the new submodule port in the net segments:
		insert_port (
			module			=> module_cursor,
			instance		=> instance,
			port			=> port_name,
			position		=> port_position,
			log_threshold	=> log_threshold + 1);
		
	end move_port;




	procedure movable_test (
	-- Tests whether the submodule port at the given point is movable. The criteria
	-- for movement are: no device, no netchanger ports there.
	-- The ONE and ONLY port allowed here is the port-to-be-dragged itself.
		module_cursor	: in pac_generic_modules.cursor;
		instance		: in pac_module_instance_name.bounded_string;
		port_name		: in pac_net_name.bounded_string;
		point 			: in type_object_position;
		log_threshold	: in type_log_level) 
	is 
		ports : type_ports;
		port : type_submodule_port;

		use pac_submodule_ports;
		use pac_device_ports;

		use et_netlists;
		use pac_netchanger_ports;

		use et_schematic_ops.nets;		
	begin
		log (text => "movable test ...", level => log_threshold);
		log_indentation_up;

		-- If no net segments start or end at given point then this test won't
		-- complain. If segments are meeting this point, no other ports must be
		-- here (except the port-to-be-dragged):
		if net_segment_at_place (module_cursor, point) then

			-- There are net segments starting or ending at point.
			-- Make sure at point are no ports of devices, netchangers or other 
			-- submodules (except the submodule port to be dragged):

			port := (instance, port_name); -- the port to be dragged, like instance OSC port 'clock_out'

			-- Collect all ports of possible other devices, submodules and netchangers
			-- at given point:
			ports := get_ports (module_cursor, point, log_threshold + 1);

			-- If no device or netchanger ports here:
			if is_empty (ports.devices) and is_empty (ports.netchangers) then

				-- If the ONE and ONLY submodule port is the 
				-- port-to-be-dragged then everything is fine.
				if length (ports.submodules) = 1 then
					
					if contains (ports.submodules, port) then
						null; -- fine -> movable test passed
					else
						-- there is another submodule port
						dragging_not_possible (to_string (port.port_name), point);
					end if;
				
				else
					-- there are more submodule ports
					dragging_not_possible (to_string (port.port_name), point);
				end if;
				
			else -- device or netchanger ports here
				dragging_not_possible (to_string (port.port_name), point);
			end if;
		end if;
		
		log_indentation_down;
	end movable_test;



	
	-- Drags the net segments according to the given netchanger ports.
	-- Changes the position of start or end points of segments.
	-- Does NOT create new connections with segments if a port
	-- lands on the start or end point of another segment.
	-- Does NOT create a new connection with a segments if a port
	-- lands between start and end point.
	procedure drag_net_segments (
		module			: in pac_generic_modules.cursor;	-- the module
		ports_before	: in et_submodules.type_netchanger_ports;	-- the old port positions
		ports_after		: in et_submodules.type_netchanger_ports;	-- the new port positions
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



	-- Drags the net segments according to the given submodule ports.
	-- Changes the position of start or end points of segments.
	-- Does NOT create new connections with segments if the port
	-- lands on the start or end point of another segment.
	-- Does NOT create a new connection with segments if the port
	-- lands between start and end point.
	procedure drag_net_segments (
		module			: in pac_generic_modules.cursor;				-- the module
		port			: in type_submodule_port;	-- instance and port name
		pos_before		: in type_object_position;	-- the old port position
		pos_after		: in type_object_position;	-- the new port position
		log_threshold	: in type_log_level) 
	is
		procedure query_nets (
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

				-- This flag goes true once the port has been found the first time
				-- and affected end points of segments have been moved.
				drag_processed : boolean := false;

				
				procedure query_segments (strand : in out type_strand) is
					use pac_net_segments;

					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure change_segment (segment : in out type_net_segment) is 
					-- Changes the position of start or end point of a segment according to the drag point.
					begin -- change_segment
						log_indentation_up;
						
						-- if port sits on a start point of a segment -> move start point
						if get_A (segment) = get_place (pos_before) then
							log (text => "move segment start point from" & 
								to_string (get_A (segment)),
								level => log_threshold + 3);

							set_A (segment, get_place (pos_after));

							log (text => "to" & 
								to_string (get_A (segment)),
								level => log_threshold + 3);

							-- signal iterations in upper level to cancel
							drag_processed := true;
						end if;

						-- if port sits on an end point of a segment -> move end point
						if get_B (segment) = get_place (pos_before) then
							log (text => "move segment end point from" & 
								to_string (get_B (segment)),
								level => log_threshold + 3);

							set_B (segment, get_place (pos_after));

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

					-- Probe all segments of strand for pos_before. This loop must not
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
					if get_sheet (element (strand_cursor).position) = get_sheet (pos_before) then
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
		log (text => "dragging net segments with submodule ports on sheet" & 
			 to_string (get_sheet (pos_before)) & " ...", level => log_threshold);
		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);
		
		log_indentation_down;
	end drag_net_segments;


	
	
	procedure drag_port (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC
		port_name		: in pac_net_name.bounded_string; -- clock_output
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is
		use et_submodules;

		-- The place where the box is in the parent module:
		submodule_position : type_object_position;

		-- Handling the absolute position of the port requires these variables:
		port_position_before : type_object_position;
		port_position_after  : type_object_position;
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
	
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			submod_cursor : pac_submodules.cursor;

			
			procedure query_ports (
				submod_name	: in pac_module_instance_name.bounded_string;
				submodule	: in out et_submodules.type_submodule) 
			is
				use et_submodules.pac_submodule_ports;
				port_cursor : et_submodules.pac_submodule_ports.cursor;

				procedure move (
					port_name	: in pac_net_name.bounded_string;
					port		: in out et_submodules.type_submodule_port)
				is
					submod_pos_tmp : type_vector_model := submodule_position.place;
					point_tmp : type_vector_model := point;
				begin
					-- BACKUP THE PORT POSITION BEFORE THE DRAG OPERATION:
					port_position_before := to_position (
								point	=> port.position, -- relative x/y to submodule position
								sheet	=> get_sheet (submodule_position)); -- same sheet as submodule box

					move_by (
						point	=> port_position_before.place,
						offset	=> submodule_position.place);
					-- Now port_position_before contains the absolute port position of 
					-- the port BEFORE the drag operation.

					-- Test whether the port at the current position can be dragged:
					movable_test (
						module_cursor		=> module_cursor,
						instance			=> instance,
						port_name			=> port_name,
						point				=> port_position_before,
						log_threshold		=> log_threshold + 1);

					-- move port along edge of box
					case coordinates is
						when ABSOLUTE =>
							-- From the given point the absolute submodule position must 
							-- be subtracted. This requires inversion of x/y of submodule position.
							-- We accompish that by mirroring along x and y axis.
							mirror_point (submod_pos_tmp, MIRROR_ALONG_X_AXIS);
							mirror_point (submod_pos_tmp, MIRROR_ALONG_Y_AXIS);

							-- Subtract from given point the absolute submodule position:
							move_by (
								point	=> point_tmp,
								offset	=> submod_pos_tmp);

							-- assign the new port position
							port.position := point_tmp;

						when RELATIVE =>
							move_by (
								point	=> port.position,
								offset	=> point);
							
					end case;

					
					-- The port must be somewhere at the edge of the box
					-- of the submodule. The port position is relative to 
					-- the lower left corner of the box:
					if at_edge (port.position, submodule.size) then

						-- Later, for inserting the new port in the nets the
						-- absolute port position must be built:
						port_position_after := to_position (
									point	=> port.position, -- relative x/y to submodule position
									sheet	=> get_sheet (submodule_position)); -- same sheet as submodule box

						move_by (
							point	=> port_position_after.place,
							offset	=> submodule_position.place);
						-- Now port_position_after contains the absolute port position of 
						-- the port AFTER the drag operation.

					else
						port_not_at_edge (port_name);
					end if;
					
				end move;
								

			begin -- query_ports
				-- Test whether the submodule provides the given port.
				port_cursor := find (submodule.ports, port_name);

				-- If the port is available (at the edge of the box) then
				-- it can be moved:
				if port_cursor /= et_submodules.pac_submodule_ports.no_element then
									
					update_element (
						container	=> submodule.ports,
						position	=> port_cursor,
						process		=> move'access);
					
				else
					submodule_port_not_found (port_name);
				end if;					
			end query_ports;


			use pac_submodules;
			
			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For moving the submodule port
				-- we take a copy of the coordinates of the submodule (the box):
				submodule_position := element (submod_cursor).position;

				log_indentation_up;

				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> query_ports'access);

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;

		
	begin -- drag_port
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" dragging port " & enclose_in_quotes (to_string (port_name)) &
					" to" & to_string (point),
					level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" dragging port " & enclose_in_quotes (to_string (port_name)) &
					" by" & to_string (point),
					level => log_threshold);

		end case;

		-- locate module
		module_cursor := locate_module (module_name);

		-- move the port along the edge of the box:
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		drag_net_segments (
			module			=> module_cursor,
			port			=> (instance, port_name),
			pos_before		=> port_position_before,
			pos_after		=> port_position_after,
			log_threshold	=> log_threshold + 1);
		
	end drag_port;

	


	-- Inserts the given netchanger ports in the net segments.
	-- If a port lands on either the start or end point of a segment, it will
	-- be regarded as "connected" with the segment.
	-- If a ports lands between start or end point of a segment, nothing happens
	-- because the docking to net segments is possible on segment ends/starts only.
	-- CS: Automatic splitting the segment into two and placing a junction is not supported
	-- jet and probably not a good idea.
	procedure insert_ports (
		module			: in pac_generic_modules.cursor;		-- the module
		index			: in et_submodules.type_netchanger_id;	-- the netchanger id
		ports			: in et_submodules.type_netchanger_ports; -- the ports to be inserted
		sheet			: in type_sheet;	-- the sheet to look at
		log_threshold	: in type_log_level) 
	is
		use et_submodules;
		
		procedure query_nets (
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
				log (text => "at" & to_string (port), level => log_threshold + 2);
				
				while not port_processed and net_cursor /= pac_nets.no_element loop
					
					update_element (
						container	=> module.nets,
						position	=> net_cursor,
						process		=> query_strands'access);
				
					next (net_cursor);
				end loop;

				log_indentation_down;
			end probe_port;

			
		begin -- query_nets
			log (text => "master port", level => log_threshold + 1);
			probe_port (ports.master, MASTER);

			log (text => "slave port", level => log_threshold + 1);			
			probe_port (ports.slave, SLAVE);
		end query_nets;

		
	begin
		log (text => "inserting netchanger ports in nets on sheet" & 
			 to_string (sheet) & " ...", level => log_threshold);
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

		log_indentation_down;
	end insert_ports;




	-- Returns the next available netchanger index in the module.
	function next_netchanger_index (
		module_cursor	: in pac_generic_modules.cursor)
		return et_submodules.type_netchanger_id 
	is
		use et_submodules;
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
		index			: in et_submodules.type_netchanger_id) -- 1, 2, 3, ...
		return boolean 
	is
		result : boolean := false; -- to be returned, goes true once the target has been found		

		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) is
			use et_submodules.pac_netchangers;
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
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		port			: in et_submodules.type_netchanger_port_name; -- SLAVE/MASTER
		log_threshold	: in type_log_level)
		return type_object_position
	is
		use et_submodules;
		port_position : type_object_position; -- to be returned		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being inquired

		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			nc_cursor : pac_netchangers.cursor;
			nc_position : type_object_position;
			port_xy : type_vector_model;

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

		
		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module)
		is
			use et_submodules;
			cursor : pac_netchangers.cursor;
			index : type_netchanger_id;
			netchanger : type_netchanger;
			inserted : boolean;
			ports : type_netchanger_ports;

			use pac_netchangers;
			
		begin -- query_netchangers

			-- set the index to be used for the new netchanger
			index := next_netchanger_index (module_cursor);
			log (text => "netchanger index is" & to_string (index), level => log_threshold + 1);
			
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
				module			=> module_cursor,
				index			=> index,
				ports			=> ports,
				sheet			=> get_sheet (place),
				log_threshold	=> log_threshold + 1);
			
		end query_netchangers;

		
	begin -- add_netchanger
		log (text => "module " & to_string (module_name) &
			" adding netchanger at" & to_string (position => place) &
			" rotation" & to_string (get_rotation (place)),
			level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_netchangers'access);
		
		log_indentation_down;		
	end add_netchanger;



	
	
	procedure drag_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is
		use et_submodules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure movable_test (
		-- Tests whether the given netchanger ports of the netchanger at location 
		-- are movable. 
		-- The criteria for movement are: no device, no submodule ports there.
		-- The ports allowed here are the ports-to-be-dragged itself.
			location 			: in type_object_position; -- only sheet number matters
			netchanger_ports	: in et_submodules.type_netchanger_ports) -- x/y of master and slave port
		is			

			
			procedure test_point (
				point		: in type_object_position; -- sheet/x/y -- the point to be probed
				port_name	: in et_submodules.type_netchanger_port_name) -- master/slave
			is 
				use et_netlists;
				ports : type_ports;
				port : type_port_netchanger;

				use et_net_ports.pac_submodule_ports;
				use pac_device_ports;
				use pac_netchanger_ports;
				use et_schematic_ops.nets;
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
						set (location, point);

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
					module			=> module_cursor,
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

	


	-- Deletes ports of the given netchanger in nets:
	procedure delete_ports (
		module			: in pac_generic_modules.cursor;			-- the module
		index			: in et_submodules.type_netchanger_id;	-- the netchanger id
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
						use et_submodules;

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
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is
		use et_submodules;
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
					module			=> module_cursor,
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






	-- Rotates the given netchanger. Disconnects it from
	-- start or end points of net segments.
	procedure rotate_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		coordinates		: in type_coordinates; -- relative/absolute
		rotation		: in et_schematic_geometry.type_rotation_model; -- 90
		log_threshold	: in type_log_level) 
	is
		use et_submodules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			cursor : pac_netchangers.cursor;
			location : type_object_position;
			rotation : et_schematic_geometry.type_rotation_model;
			ports_old : type_netchanger_ports;
			ports_new : type_netchanger_ports;

			
			procedure rotate (
				index		: in type_netchanger_id;
				netchanger	: in out type_netchanger) is
			begin
				set (netchanger.position_sch, rotation);
			end;


			use pac_netchangers;
			
			
		begin -- query_netchangers

			-- locate given netchanger
			cursor := find (module.netchangers, index);

			if cursor /= pac_netchangers.no_element then 
				-- netchanger exists

				log_indentation_up;

				-- Before the actual rotation, the coordinates of the
				-- netchanger ports must be fetched.
				ports_old := netchanger_ports (cursor);
			
				-- Fetch the current netchanger position and rotation:
				location := element (cursor).position_sch;
				rotation := get_rotation (location);

				-- Delete netchanger ports in nets:
				delete_ports (
	 				module			=> module_cursor,
					index			=> index,

					-- Get sheet number from location:
					sheet			=> get_sheet (location),
					
					log_threshold	=> log_threshold + 1);
				
				-- calculate the rotation the netchanger will have AFTER the move:
				case coordinates is
					when ABSOLUTE =>
						rotation := rotate_netchanger.rotation;

					when RELATIVE =>
						rotation := add (rotation, rotate_netchanger.rotation);
				end case;

				-- rotate the netchanger to the new rotation
				update_element (
					container	=> module.netchangers,
					position	=> cursor,
					process		=> rotate'access);

				-- Get the NEW absolute positions of the netchanger ports AFTER
				-- the rotation according to location and rotation in schematic.
				ports_new := netchanger_ports (cursor);

				-- Inserts the netchanger ports in the net segments.
				insert_ports (
					module			=> module_cursor,
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
		
		
	begin -- rotate_netchanger
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					 " rotating netchanger" & to_string (index) &
					 " to" & to_string (rotation), level => log_threshold);

			when RELATIVE =>
				if rotation in type_rotation_relative then
					log (text => "module " & to_string (module_name) &
						" rotating netchanger" & to_string (index) &
						" by" & to_string (rotation), level => log_threshold);
				else
					relative_rotation_invalid;
				end if;
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_netchangers'access);

	end rotate_netchanger;

	


	


	procedure delete_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		index			: in et_submodules.type_netchanger_id; -- 1,2,3,...
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module
		use et_submodules;

		
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

	




	-- Adds a submodule instance to the schematic.
	procedure add_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.pac_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		position		: in type_object_position; -- sheet, lower left corner x/y 
		size			: in et_submodules.type_submodule_size; -- the size of the box in x and y
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_directory_and_file_ops;
		full_file_name : constant string := expand (et_submodules.to_string (file));

		use et_module_ops;
		use et_submodules;

		
		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			submod_cursor : pac_submodules.cursor;
			inserted : boolean;
			submodule : et_submodules.type_submodule;
			use pac_submodules;
		begin -- add
			-- THE FOLLOWING IS ABOUT THE GRAPHICAL REPRESENTATION OF A SUBMODULE.
			-- THIS IS THE RECTANGULAR BOX AT THE SHEET WHERE IT THE SUBMODULE IS INSTANTIATED.
			
			-- initialize the submodule with basic properties
			submodule.file := file;
			submodule.position := position;
			submodule.size := size;
			-- Other properties like view mode, device name offset and ports
			-- have to be set via other commands.

			insert (
				container	=> module.submods,
				key			=> instance,
				new_item	=> submodule,
				position	=> submod_cursor,
				inserted	=> inserted);

			if not inserted then
				log (ERROR, "submodule instance " &
					enclose_in_quotes (to_string (instance)) &
					" already exists !", console => true);
				raise constraint_error;
			end if;
		end add;

		use pac_module_name;
		
		
	begin -- add_submodule
		log (text => "module " & enclose_in_quotes (pac_module_name.to_string (module_name)) &
			" adding submodule " & to_string (file) & 
			" instance " & enclose_in_quotes (to_string (instance)),
			level => log_threshold);

		log (text => " at" & to_string (position => position) &
			to_submodule_size (size),
			level => log_threshold);

		-- Make sure the parent module does not use itself as submodule:
		if module_name = to_module_name (file) then
			log (ERROR, "Circular dependency: A module can not have itself as submodule !", console => true);
		end if;

		-- locate module
		module_cursor := locate_module (module_name);

		-- Make sure the submodule file exists. The file is 
		-- identified by its full path and name. If the file exists
		-- then a submodule is inserted in the targeted module.
		-- NOTE: This is the rectangular box at the targeted sheet that
		-- represents the submodule:
		if ada.directories.exists (full_file_name) then
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> add'access);

		else
			log (ERROR, "submodule file " & to_string (file) & " not found !",
				 console => true);
			raise constraint_error;
		end if;

		-- THIS IS ABOUT THE ACTUAL SCHEMATIC AND LAYOUT STUFF OF THE SUBMODULE:
		-- Read the submodule file and store it as generic module:
		read_module (to_string (file), log_threshold + 1);		

	end add_submodule;




	-- Deletes all references to the given submodule in the nets.
	procedure delete_ports (
		module_cursor	: in pac_generic_modules.cursor;					-- the module
		instance		: in pac_module_instance_name.bounded_string; -- the submodule instance
		position		: in type_object_position; 		-- the location in the schematic (only sheet matters)
		log_threshold	: in type_log_level)
	is
		
		-- Removes all references to the submodule instance from the net segments.
		procedure query_nets (
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

				
				procedure query_segments (strand : in out type_strand) is
					use pac_net_segments;
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					
					procedure change_segment (
						segment : in out type_net_segment) 
					is begin
						delete_submodule_ports (segment, instance);
					end change_segment;

					
				begin -- query_segments
					log_indentation_up;

					while segment_cursor /= pac_net_segments.no_element loop
						log (text => "probing " & to_string (segment_cursor), level => log_threshold + 2);
						
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
				
				while strand_cursor /= pac_strands.no_element loop

					-- We pick out only the strands on the targeted sheet:
					if get_sheet (element (strand_cursor).position) = get_sheet (position) then
						log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);

						log_indentation_up;
						log (text => "strand " & to_string (position => element (strand_cursor).position),
							level => log_threshold + 1);

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

			
		begin -- query_nets
			while net_cursor /= pac_nets.no_element loop
				
				update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);
			
				next (net_cursor);
			end loop;
		end query_nets;

		
	begin -- delete_ports
		log (text => "deleting submodule ports in nets ...", level => log_threshold);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_nets'access);

	end delete_ports;


	

	
	procedure delete_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level) 
	is
		use et_submodules;

		-- The place where the box is in the parent module:
		submodule_position : type_object_position;
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use pac_submodules;
			submod_cursor : pac_submodules.cursor;
		begin
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For removing the submodule ports
				-- we take a copy of the coordinates of the submodule (the box):
				submodule_position := element (submod_cursor).position;

				-- delete the submodule (the box)
				delete (module.submods, submod_cursor);
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;
		
	
	begin -- delete_submodule
		log (text => "module " & to_string (module_name) &
			" deleting submodule instance " & enclose_in_quotes (to_string (instance)),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- load submodule_position and delete submodule
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- delete all references to the submodule in the nets
		delete_ports (
			module_cursor	=> module_cursor,
			instance		=> instance,
			position		=> submodule_position,
			log_threshold	=> log_threshold + 1);
		
	end delete_submodule;


	


	procedure move_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute
		sheet			: in type_sheet_relative; -- -3/0/2
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is
		use et_submodules;

		-- The place where the box is in the parent module BEFORE and AFTER the move:
		submodule_position_before : type_object_position;
		submodule_position_after : type_object_position;		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			submod_cursor : pac_submodules.cursor;

			-- the submodule ports to be moved
			ports : et_submodules.pac_submodule_ports.map; -- port names and relative x/y positions

			procedure move (
				instance	: in pac_module_instance_name.bounded_string;
				submodule	: in out et_submodules.type_submodule) is
			begin
				case coordinates is
					when ABSOLUTE =>
						submodule.position := to_position (point, sheet);

					when RELATIVE =>
						move (
							position	=> submodule.position,
							offset		=> to_position_relative (point, sheet)
							);
				end case;

				-- store new submodule position
				submodule_position_after := submodule.position;

				exception
					when event: others =>
						log (ERROR, "coordinates invalid !", console => true); -- CS required more details
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;
				
			end move;

			
			-- Inserts the ports into the nets. The sheet number is taken
			-- from the submodule_position_after (or submodule_position_before).
			procedure insert_ports is 
				use et_submodules.pac_submodule_ports;
				port_cursor : et_submodules.pac_submodule_ports.cursor := ports.first;
				position : type_object_position;
			begin
				while port_cursor /= et_submodules.pac_submodule_ports.no_element loop

					-- build the port position (sheet/x/y)
					position := to_position 
							(
							point	=> element (port_cursor).position,
							sheet	=> get_sheet (submodule_position_after)
							);

					-- insert the port
					insert_port (
						module			=> module_cursor,
						instance		=> instance, -- OSC1
						port			=> key (port_cursor), -- port name like CE
						position		=> position, -- sheet/x/y
						log_threshold	=> log_threshold + 1);
					
					next (port_cursor);
				end loop;
			end insert_ports;

			
			use pac_submodules;
			
			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				-- For moving the submodule ports
				-- we take a copy of the coordinates of the submodule (the box)
				-- BEFORE the move operation:
				submodule_position_before := element (submod_cursor).position;

				log_indentation_up;
				
				-- delete all references to the submodule in the nets
				delete_ports (
					module_cursor	=> module_cursor,
					instance		=> instance,
					position		=> submodule_position_before, -- only sheet number matters
					log_threshold	=> log_threshold + 1);

				-- move the submodule (the box). Load submodule_position_after
				-- with the coordinates AFTER the move operation:
				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> move'access);
				
				-- Get the port positions relative to the lower left 
				-- corner of the submodule box.
				ports := element (submod_cursor).ports;

				-- calculate the absolute port positions AFTER the move:
				et_submodules.move_ports (ports, submodule_position_after);

				-- ports now provides port names and absoltue x/y positions.
				-- The new ports will be inserted in the nets now:
				insert_ports;

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;

		
	begin -- move_submodule
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving submodule instance " & to_string (instance) &
					" to sheet" & to_string (sheet) &
					to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving submodule instance " & to_string (instance) &
					" by " & relative_to_string (sheet) & " sheet(s)" &
					to_string (point), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);
		
	end move_submodule;

	

	


	procedure drag_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
		use et_submodules;

		-- This type describes a submodule port before and after the drag operation:
		type type_drag is record
			name	: pac_net_name.bounded_string;
			before	: type_object_position;
			after 	: type_object_position;
		end record;

		-- Since there are lots of submodule ports we store the drag points in a simple list:
		package type_drags is new doubly_linked_lists (type_drag);
		drag_list : type_drags.list;

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			submod_cursor : pac_submodules.cursor;

			-- the submodule ports to be moved
			use et_submodules.pac_submodule_ports;
			ports : et_submodules.pac_submodule_ports.map; -- port names and relative x/y positions
			port_cursor : et_submodules.pac_submodule_ports.cursor := ports.first;

			procedure query_ports (
				submod_name	: in pac_module_instance_name.bounded_string;
				submodule	: in et_submodules.type_submodule) 
			is
				port_cursor : et_submodules.pac_submodule_ports.cursor := submodule.ports.first;

				procedure build_drag_point (
					port_name	: in pac_net_name.bounded_string;
					port		: in et_submodules.type_submodule_port) 
				is
					drag : type_drag;
				begin
					-- Set the name of the drag according to the port name:
					drag.name := port_name; -- CE, WE, ...
					
					-- Build and the absolute port position BEFORE the drag operation.
					-- The result is stored in drag.before.
					drag.before := submodule.position;

					move_by (
						point	=> drag.before.place,
						offset	=> port.position);
			
					-- Now drag.before contains the absolute port position of 
					-- the port BEFORE the drag operation.

					-- Test whether the port at the current position can be dragged:
					movable_test (
						module_cursor		=> module_cursor,
						instance			=> instance,
						port_name			=> port_name,
						point				=> drag.before,
						log_threshold		=> log_threshold + 1);

					-- Compute the absolute port position on the sheet AFTER the drag operation.
					-- The result is stored in drag.after:
					case coordinates is
						when ABSOLUTE =>

							drag.after := to_position (
								point	=> point,
								sheet	=> get_sheet (submodule.position));

						when RELATIVE =>

							drag.after := submodule.position;

							move_by (
								point	=> drag.after.place,
								offset	=> point);

					end case;

					move_by (
						point	=> drag.after.place,
						offset	=> port.position);

					-- Now drag.after contains the absolute port position of 
					-- the port AFTER the drag operation.
					
					type_drags.append (drag_list, drag);
					
				end build_drag_point;

				
			begin -- query_ports
				while port_cursor /= et_submodules.pac_submodule_ports.no_element loop

					query_element (
						position	=> port_cursor,
						process		=> build_drag_point'access);
										
					next (port_cursor);
				end loop;
			end query_ports;

			
			-- Moves the box on the sheet according to given target position.
			procedure move_box (
				submod_name	: in pac_module_instance_name.bounded_string;
				submodule	: in out et_submodules.type_submodule) 
			is begin
				-- NOTE: The sheet number does not change in drag operations.
				case coordinates is
					when ABSOLUTE =>
						set (submodule.position, point);

					when RELATIVE =>
						move_by (
							point	=> submodule.position.place,
							offset	=> point);
				end case;

				exception
					when event: others =>
						log (ERROR, "coordinates invalid !", console => true); -- CS required more details
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;

			end move_box;


			use pac_submodules;

			
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				log_indentation_up;

				-- build the drag list (movable_test included)
				query_element (
					position	=> submod_cursor,
					process		=> query_ports'access);
				
				-- move the submodule (the box):
				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> move_box'access);

				log_indentation_down;				
			else
				submodule_not_found (instance);
			end if;
		end query_submodules;

		
		procedure drag_segments is
		-- Drags the net segments according to the drag_list that has been
		-- created earlier.
			use type_drags;
			drag_cursor : type_drags.cursor := drag_list.first;
		begin
			while drag_cursor /= type_drags.no_element loop

				drag_net_segments (
					module			=> module_cursor,
					port			=> (instance, element (drag_cursor).name),
					pos_before		=> element (drag_cursor).before,
					pos_after		=> element (drag_cursor).after,
					log_threshold	=> log_threshold + 1);
				
				next (drag_cursor);
				
			end loop;
		end drag_segments;

		
	begin -- drag_submodule
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" dragging submodule instance " & to_string (instance) &
					" to" & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" dragging submodule instance " & to_string (instance) &
					" by " & to_string (point), level => log_threshold);
		end case;
		
		-- locate module
		module_cursor := locate_module (module_name);

		-- move the ports of the submodule,
		-- create drag_list
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		-- drag the connected net segments (by drag_list)
		drag_segments;

	end drag_submodule;



	
	
	procedure copy_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_origin	: in pac_module_instance_name.bounded_string; -- OSC1
		instance_new	: in pac_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		destination		: in type_object_position; -- sheet/x/y
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
		use et_submodules;

		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			submod_cursor : pac_submodules.cursor;
			inserted : boolean;
			submodule : et_submodules.type_submodule;

			-- the submodule ports to be inserted in the nets
			ports : et_submodules.pac_submodule_ports.map; -- port names and relative x/y positions

			use pac_submodules;
			
			
			-- Inserts the ports into the nets. The sheet number is taken
			-- from the submodule position.
			procedure insert_ports is 
				use et_submodules.pac_submodule_ports;
				port_cursor : et_submodules.pac_submodule_ports.cursor := ports.first;
				position : type_object_position;
			begin
				while port_cursor /= et_submodules.pac_submodule_ports.no_element loop

					-- build the port position (sheet/x/y)
					position := to_position 
							(
							point	=> element (port_cursor).position,
							sheet	=> get_sheet (element (submod_cursor).position)
							);

					-- insert the port
					insert_port (
						module			=> module_cursor,
						instance		=> instance_new, -- CLOCK_GENERATOR
						port			=> key (port_cursor), -- port name like CE
						position		=> position, -- sheet/x/y
						log_threshold	=> log_threshold + 1);
					
					next (port_cursor);
				end loop;
			end insert_ports;
			
			
		begin -- query_submodules
			-- locate the submodule of origin
			if contains (module.submods, instance_origin) then

				submod_cursor := find (module.submods, instance_origin); -- the submodule of origin should be there

				-- THE FOLLOWING IS ABOUT THE GRAPHICAL REPRESENTATION OF A SUBMODULE.
				-- THIS IS THE RECTANGULAR BOX AT THE SHEET WHERE IT THE SUBMODULE IS INSTANTIATED.

				-- copy submodule of origin to temporarily submodule
				submodule := element (submod_cursor); 
 
				-- overwrite position as given by destination
				submodule.position := destination;

				-- Overwrite position in schematic by zero so that the new instance sits at 
				-- the lower left corner of the layout drawing:
				submodule.position_in_board := et_board_coordinates.pac_geometry_2.origin_zero_rotation;

				insert (
					container	=> module.submods,
					key			=> instance_new,
					position	=> submod_cursor,
					inserted	=> inserted,
					new_item	=> submodule);
					
				if not inserted then
					log (ERROR, "submodule instance " &
						enclose_in_quotes (to_string (instance_new)) &
						" already exists !", console => true);
					raise constraint_error;
				end if;
				
				log_indentation_up;

				-- Get the port positions of the new instance relative to the lower left 
				-- corner of the submodule box.
				ports := element (submod_cursor).ports;

				-- calculate the absolute port positions:
				et_submodules.move_ports (ports, element (submod_cursor).position);

				-- ports now provides port names and absoltue x/y positions.
				-- The new ports will be inserted in the nets now:
				insert_ports;

				log_indentation_down;				
			else
				submodule_not_found (instance_origin);
			end if;
		end query_submodules;
		
		
	begin -- copy_submodule
		log (text => "module " & to_string (module_name) &
			 " copying submodule instance " & enclose_in_quotes (to_string (instance_origin)) &
			 " to instance " & enclose_in_quotes (to_string (instance_new)) &
			" at" & et_schematic_coordinates.to_string (position => destination), level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);
		
	end copy_submodule;

	

	
	
	procedure rename_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance_old	: in pac_module_instance_name.bounded_string; -- OSC1
		instance_new	: in pac_module_instance_name.bounded_string; -- CLOCK_GENERATOR
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
		use et_submodules;

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			submod_cursor : pac_submodules.cursor;
			submodule_old : et_submodules.type_submodule;

			-- the submodule ports to be inserted in the nets
			ports : et_submodules.pac_submodule_ports.map; -- port names and relative x/y positions

			
			procedure insert_ports is 
			-- Inserts the ports into the nets. The sheet number is taken
			-- from the submodule position.
				use et_submodules.pac_submodule_ports;
				port_cursor : et_submodules.pac_submodule_ports.cursor := submodule_old.ports.first;
				position : type_object_position;
			begin
				while port_cursor /= et_submodules.pac_submodule_ports.no_element loop

					-- build the port position (sheet/x/y)
					position := to_position 
							(
							point	=> element (port_cursor).position,
							sheet	=> get_sheet (submodule_old.position)
							);

					-- insert the port
					insert_port (
						module			=> module_cursor,
						instance		=> instance_new, -- CLOCK_GENERATOR
						port			=> key (port_cursor), -- port name like CE
						position		=> position, -- sheet/x/y
						log_threshold	=> log_threshold + 1);
					
					next (port_cursor);
				end loop;
			end insert_ports;


			use pac_submodules;
			
			
		begin -- query_submodules
			-- locate the submodule to be renamed
			if contains (module.submods, instance_old) then
				submod_cursor := find (module.submods, instance_old); -- the submodule should be there

				-- take a copy of the old submodule
				submodule_old := element (submod_cursor);

				-- insert the old module with the new name in the module list
				insert (
					container	=> module.submods,
					key			=> instance_new,
					new_item	=> submodule_old);

				-- delete all references to the old submodule in the nets
				delete_ports (
					module_cursor	=> module_cursor,
					instance		=> instance_old,
					position		=> submodule_old.position,
					log_threshold	=> log_threshold + 1);

				-- calculate the absolute port positions:
				et_submodules.move_ports (submodule_old.ports, submodule_old.position);
				
				-- submodule_old.ports provides port names and absolute x/y positions.
				-- The new ports will be inserted in the nets now:
				insert_ports;

				-- delete the old submodule in the module list
				delete (
					container	=> module.submods,
					position	=> submod_cursor);

			else
				submodule_not_found (instance_old);
			end if;
		end query_submodules;
		
		
	begin -- rename_submodule
		log (text => "module " & to_string (module_name) &
			 " renaming submodule instance " & enclose_in_quotes (to_string (instance_old)) &
			 " to " & to_string (instance_new),
			 level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- The new name must not be in use already:
		if submodule_instance_exists (module_cursor, instance_new) then
			log (ERROR, "submodule instance " & enclose_in_quotes (to_string (instance_new)) &
				 " already exists !", console => true);
			raise constraint_error;
		else
			
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_submodules'access);

		end if;
		
	end rename_submodule;



	
	procedure mount_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_parent	: in pac_assembly_variant_name.bounded_string; -- low_cost								  
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		variant_submod	: in pac_assembly_variant_name.bounded_string; -- fixed_frequency
		log_threshold	: in type_log_level) 
	is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure query_variants (
		-- Locates the targeted assembly variant of the parent module.
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			use et_assembly_variants.pac_assembly_variants;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;

			
			procedure mount (
				name		: in pac_assembly_variant_name.bounded_string; -- low_cost (parent module)
				variant		: in out et_assembly_variants.type_assembly_variant) is
				use et_assembly_variants.pac_submodule_variants;
				cursor : pac_submodule_variants.cursor;
			begin
				-- Locate the submodule instance in the variant of the parent module.
				-- If already there, delete it and insert it anew
				-- as specified by the operator.
				cursor := find (variant.submodules, instance);

				if cursor /= pac_submodule_variants.no_element then -- submodule already in assembly variant
					delete (variant.submodules, cursor); -- delete submodule instance
				end if;

				-- insert submodule instance anew with given submodule variant
				insert (
					container	=> variant.submodules,
					key			=> instance, -- OSC1
					new_item	=> (variant => variant_submod) -- fixed_frequency
					);
				
			end mount;
			
			
		begin -- query_variants
			-- the variant (low_cost) must exist in the parent module
			cursor := et_assembly_variants.pac_assembly_variants.find (module.variants, variant_parent);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then

				-- Insert the submodule instance with the desired variant:
				et_assembly_variants.pac_assembly_variants.update_element (
					container	=> module.variants,
					position	=> cursor,
					process		=> mount'access);

			else
				assembly_variant_not_found (variant_parent);
			end if;
		end query_variants;

		
	begin -- mount_submodule
		log (text => "module " & to_string (module_name) &
			 " variant " & enclose_in_quotes (to_variant (variant_parent)) &
			 " submodule instance " & enclose_in_quotes (to_string (instance)) &
			 " mounting variant " & enclose_in_quotes (to_variant (variant_submod)),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- Test whether the given parent module contains the given submodule instance (OSC1)
		if submodule_instance_exists (module_cursor, instance) then

			if assembly_variant_exists (module_cursor, instance, variant_submod) then
			
				update_element (
					container	=> generic_modules,
					position	=> module_cursor,
					process		=> query_variants'access);
			else
				log (ERROR, "submodule instance " &
					 enclose_in_quotes (to_string (instance)) &
					 " does not provide assembly variant " &
					 enclose_in_quotes (to_variant (variant_submod)) & " !",
					 console => true);
				raise constraint_error;
			end if;

		else
			submodule_not_found (instance);
		end if;
		
	end mount_submodule;

	


	
	procedure remove_submodule (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		variant_parent	: in pac_assembly_variant_name.bounded_string; -- low_cost
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level) 
	is

		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;

		procedure query_variants (
		-- Locates the targeted assembly variant of the parent module.
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is
			use et_assembly_variants.pac_assembly_variants;
			cursor : et_assembly_variants.pac_assembly_variants.cursor;

			
			procedure remove (
				name		: in pac_assembly_variant_name.bounded_string; -- low_cost (parent module)
				variant		: in out et_assembly_variants.type_assembly_variant) is
				use et_assembly_variants.pac_submodule_variants;
				cursor : pac_submodule_variants.cursor;
			begin
				-- Locate the submodule instance in the variant of the parent module.
				-- Issue error message if not found.
				cursor := find (variant.submodules, instance);

				if cursor /= pac_submodule_variants.no_element then -- submodule in assembly variant
					delete (variant.submodules, cursor); -- delete submodule instance
				else
					log (ERROR, "submodule " & to_string (instance) &
						" not found in assembly variant " &
						enclose_in_quotes (to_variant (variant_parent)) & " !",
						 console => true);
					raise constraint_error;
				end if;
			end remove;

			
		begin -- query_variants
			-- the variant (low_cost) must exist in the parent module
			cursor := et_assembly_variants.pac_assembly_variants.find (module.variants, variant_parent);

			if cursor /= et_assembly_variants.pac_assembly_variants.no_element then

				-- Remove the submodule instance
				et_assembly_variants.pac_assembly_variants.update_element (
					container	=> module.variants,
					position	=> cursor,
					process		=> remove'access);

			else
				assembly_variant_not_found (variant_parent);
			end if;

		end query_variants;

		
	begin -- remove_submodule
		log (text => "module " & to_string (module_name) &
			" variant " & enclose_in_quotes (to_variant (variant_parent)) &
			" removing variant of submodule instance " & enclose_in_quotes (to_string (instance)),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- Test whether the given parent module contains the given submodule instance (OSC1)
		if submodule_instance_exists (module_cursor, instance) then

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_variants'access);

		else
			submodule_not_found (instance);
		end if;
		
	end remove_submodule;





	function submodule_instance_exists (
		module		: in pac_generic_modules.cursor; -- the parent module that contains the submodule instance
		instance	: in pac_module_instance_name.bounded_string) -- OSC1
		return boolean
	is

		instance_found : boolean := false; -- to be returned

		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_submodules.pac_submodules;
		begin
			if contains (module.submods, instance) then
				instance_found := true;
			end if;
		end query_submodules;

		
	begin
		-- search in the parent module for the given submodule instance
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_submodules'access);

		return instance_found;
	end submodule_instance_exists;


	



	function assembly_variant_exists (
		module		: in pac_generic_modules.cursor; -- the parent module that contains the submodule instance
		instance	: in pac_module_instance_name.bounded_string; -- OSC1
		variant		: in pac_assembly_variant_name.bounded_string) -- low_cost				
		return boolean 
	is
		variant_found : boolean := false; -- to be returned

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_submodules;
			use et_submodules.pac_submodules;
			submod_instance_cursor : et_submodules.pac_submodules.cursor;
			submod_path : pac_submodule_path.bounded_string;
			submod_name	: pac_module_name.bounded_string;
			submod_cursor : pac_generic_modules.cursor;

			
			-- Locates the given assembly variant in the submodule.
			-- Sets flag variant_found.
			procedure query_variants (
				submodule_name	: in pac_module_name.bounded_string;
				submodule		: in type_generic_module)
			is
				use et_assembly_variants;
			begin
				if pac_assembly_variants.contains (submodule.variants, variant) then
					variant_found := true;
				end if;
			end query_variants;
			
				
		begin -- query_submodules
			-- locate the submodule instance by the given instance name
			submod_instance_cursor := find (module.submods, instance);

			-- get the file name of the submodule like $ET_TEMPLATES/motor_driver.mod
			submod_path :=  element (submod_instance_cursor).file;

			-- convert the submodule path to a submodule name
			submod_name := to_module_name (remove_extension (to_string (submod_path)));

			--log (text => "submod name " & to_string (submod_name));

			-- get a cursor to the submodule file
			submod_cursor := locate_module (submod_name);

			-- locate the given variant in the submodule
			pac_generic_modules.query_element (
				position	=> submod_cursor,
				process		=> query_variants'access);

		end query_submodules;
		
		
	begin
		-- search in the parent module for the given submodule instance
		pac_generic_modules.query_element (
			position	=> module,
			process		=> query_submodules'access);

		return variant_found;
	end assembly_variant_exists;



	


	function get_alternative_submodule (
		module	: in pac_generic_modules.cursor; -- the module like motor_driver
		variant	: in pac_assembly_variant_name.bounded_string; -- low_cost				
		submod	: in pac_module_instance_name.bounded_string) -- OSC1
		return pac_submodule_variants.cursor 
	is
		cursor : pac_submodule_variants.cursor; -- to be returned;

		
		procedure query_variants (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_assembly_variants;

			variant_cursor : pac_assembly_variants.cursor;

			
			procedure query_submodules (
				variant_name	: in pac_assembly_variant_name.bounded_string;
				variant			: in type_assembly_variant) 
			is
				use pac_submodule_variants;
			begin
				cursor := find (variant.submodules, submod);
			end query_submodules;

			
		begin -- query_variants
			variant_cursor := find (module.variants, variant);

			query_element (
				position	=> variant_cursor,
				process		=> query_submodules'access);
		end;

		
	begin -- get_alternative_submodule
		if is_default (variant) then
			cursor := pac_submodule_variants.no_element;
		else
			pac_generic_modules.query_element (
				position	=> module,
				process		=> query_variants'access);
		end if;
		
		return cursor;
	end get_alternative_submodule;



	
	
	
	procedure set_submodule_file (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.pac_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_directory_and_file_ops;
		full_file_name : constant string := expand (et_submodules.to_string (file));

		use et_module_ops;
		use et_submodules;

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is
			submod_cursor : pac_submodules.cursor;

			procedure set_file (
				submod_name	: in pac_module_instance_name.bounded_string;
				submodule	: in out et_submodules.type_submodule) 
			is
				-- Prior to assigning the file, we create a test submodule and test
				-- whether it provides all the ports as required by the graphical 
				-- representation of the submodule (the box). 
				-- The test module is a copy of the targeted submodule except that it
				-- get the given file assigned. The test submodule will then be stored in a
				-- map and will be the only item in the map:
				test_mods : pac_submodules.map;
				test_mod : et_submodules.type_submodule := submodule;
				test_mod_cursor : pac_submodules.cursor;

				-- For iterating the ports of the submodule box, we need a cursor:
				use et_submodules.pac_submodule_ports;
				port_cursor : et_submodules.pac_submodule_ports.cursor := submodule.ports.first;

				use pac_submodules;

				
			begin -- set_file
				log (text => "testing ports ...", level => log_threshold + 1);
				log_indentation_up;
				
				test_mod.file := file; -- assign the file to the test submodule

				-- insert the test submodule in map test_mods:
				insert (
					container	=> test_mods,
					key			=> instance,
					new_item	=> test_mod);

				test_mod_cursor := test_mods.first;

				-- Test ports of targeted submodule whether they are provided by
				-- the test module (indicated by test_mod_cursor). If all ports
				-- have been found in the submodule schematic, overwrite the given 
				-- submodule with the test module.
				while port_cursor /= et_submodules.pac_submodule_ports.no_element loop
					log (text => to_string (key (port_cursor)), level => log_threshold + 2);

					if not submodule_port_exists (
						module		=> test_mod_cursor,
						port 		=> key (port_cursor), -- clock_output
						direction	=> element (port_cursor).direction -- master/slave
						) then
						
 						port_not_provided (key (port_cursor));
					end if;
					
					next (port_cursor);
				end loop;

				log_indentation_down;

				-- Overwrite submodule with test module:
				submodule := test_mod;
				
				exception
					when event: others =>
						log_indentation_reset;
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;
				
			end set_file;


			use pac_submodules;
			
				
		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there
				
				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> set_file'access);
				
			else
				submodule_not_found (instance);
			end if;

		end query_submodules;

		
	begin -- set_submodule_file
		log (text => "module " & to_string (module_name) &
			" setting instance " & enclose_in_quotes (to_string (instance)) &
			" file to " & to_string (file),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- Make sure the submodule file exists. The file is 
		-- identified by its full path and name. If the file exists
		-- then the given submodule instance gets the file assigned.
		-- NOTE: This is the rectangular box at the targeted sheet that
		-- represents the submodule:
		if ada.directories.exists (full_file_name) then

			-- THIS IS ABOUT THE ACTUAL SCHEMATIC AND LAYOUT STUFF OF THE SUBMODULE:
			-- Read the submodule file and store its content in container et_project.modules:
			read_module (to_string (file), log_threshold + 1);		
			
			log_indentation_up;

			-- THIS IS ABOUT THE GRAPHICAL REPRESENTATION OF THE SUBMODULE:
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> query_submodules'access);

			log_indentation_down;
		else
			log (ERROR, "submodule file " & to_string (file) & " not found !",
				 console => true);
			raise constraint_error;
		end if;
		
	end set_submodule_file;



	


	procedure check_integrity (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being checked

		errors : natural := 0;
		warnings : natural := 0;

		procedure error is begin errors := errors + 1; end;
		procedure warning is begin warnings := warnings + 1; end;

		
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_nets;

			-- Here we collect all ports of devices (like IC4 CE, R2 1, ...) across all the nets.
			-- Since device_port_collector is an ordered set, an exception will be raised if
			-- a port is to be inserted more than once. Something like IC4 port CE must
			-- occur only ONCE throughout the module.
			use pac_device_ports;
			device_port_collector : pac_device_ports.set;

			procedure collect_device_port (
				port	: in type_device_port;
				net		: in pac_net_name.bounded_string) is 
				use et_symbols;
			begin
			-- Collect device ports. exception will be raised of port occurs more than once.
				insert (device_port_collector, port);

				exception when event: others =>
					log (ERROR, "net " & to_string (net) &
						" device " & to_string (port.device_name) &
						" port " & to_string (port.port_name) &
						" already used !",
						console => true);
					-- CS: show the net, sheet, xy where the port is in use already

					log (text => ada.exceptions.exception_message (event), console => true);
			end collect_device_port;

			
			-- Here we collect all ports of submodules (like MOT_DRV reset) across all the nets.
			-- Since submodule_port_collector is an ordered set, an exception will be raised if
			-- a port is to be inserted more than once. Something like "MOT_DRV reset" must
			-- occur only ONCE throughout the module.
			use pac_submodule_ports;
			submodule_port_collector : pac_submodule_ports.set;

			
			procedure collect_submodule_port (
				port	: in type_submodule_port;
				net		: in pac_net_name.bounded_string)
			is begin
			-- Collect submodule ports. exception will be raised of port occurs more than once.
				insert (submodule_port_collector, port);

				exception when event: others =>
					log (ERROR, "net " & to_string (net) &
						" submodule " & to_string (port.module_name) &
						" port " & to_string (port.port_name) &
						" already used !",
						console => true);
					-- CS: show the net, sheet, xy where the port is in use already

					log (text => ada.exceptions.exception_message (event), console => true);
			end collect_submodule_port;
			

			-- Here we collect all ports of netchangers (like netchanger port master/slave) across all the nets.
			-- Since netchanger_ports_collector is an ordered set, an exception will be raised if
			-- a port is to be inserted more than once. Something like "netchanger port master" must
			-- occur only ONCE throughout the module.
			use et_netlists.pac_netchanger_ports;
			netchanger_ports_collector : et_netlists.pac_netchanger_ports.set;

			
			procedure collect_netchanger_port (
				port	: in et_netlists.type_port_netchanger;
				net		: in pac_net_name.bounded_string)
			is begin
			-- Collect netchanger ports. exception will be raised of port occurs more than once.
				insert (netchanger_ports_collector, port);

				exception when event: others =>
					log (ERROR, "net " & to_string (net) &
						" netchanger" & et_submodules.to_string (port.index) &
						" port" & et_submodules.to_string (port.port) &
						" already used !",
						console => true);
					-- CS: show the net, sheet, xy where the port is in use already

					log (text => ada.exceptions.exception_message (event), console => true);
			end collect_netchanger_port;


			procedure query_net (net_cursor : in pac_nets.cursor) is
				use pac_net_name;

				procedure query_strands (
					net_name	: in pac_net_name.bounded_string;
					net			: in type_net) 
				is

					use pac_strands;
					
					procedure query_strand (strand_cursor : in pac_strands.cursor) is

						procedure query_segments (strand : in type_strand) is
							use pac_net_segments;

							procedure query_segment (segment_cursor : in pac_net_segments.cursor) is

								procedure query_ports_devices (segment : in type_net_segment) is
									
									procedure query_port (port_cursor : in pac_device_ports.cursor) is 
										use et_symbols;
									begin
										log (text => "device " & to_string (element (port_cursor).device_name) &
											 " port " & to_string (element (port_cursor).port_name), level => log_threshold + 4);

										if not device_port_exists (
											module_cursor	=> module_cursor,
											device_name		=> element (port_cursor).device_name,
											port_name		=> element (port_cursor).port_name) then

											error;
											
											log (ERROR, "device " & to_string (element (port_cursor).device_name) &
												 " port " & to_string (element (port_cursor).port_name) &
												 " does not exist !");
										end if;

										collect_device_port (port => element (port_cursor), net => net_name);
									end query_port;
										
								begin
									log_indentation_up;
									iterate (segment.ports.A.devices, query_port'access);
									iterate (segment.ports.B.devices, query_port'access);
									log_indentation_down;
								end query_ports_devices;


								
								procedure query_ports_submodules (segment : in type_net_segment) is
									
									procedure query_port (port_cursor : in pac_submodule_ports.cursor) is begin
										log (text => "submodule " & to_string (element (port_cursor).module_name) &
											 " port " & pac_net_name.to_string (element (port_cursor).port_name), level => log_threshold + 4);

										if not submodule_port_exists (
											module_cursor	=> module_cursor,
											submod_instance	=> element (port_cursor).module_name, -- MOT_DRV_3
											port_name		=> element (port_cursor).port_name) then -- RESET

											error;
											
											log (ERROR, "submodule " & to_string (element (port_cursor).module_name) &
												 " port " & pac_net_name.to_string (element (port_cursor).port_name) &
												 " does not exist !");
										end if;

										collect_submodule_port (port => element (port_cursor), net => net_name);
									end query_port;
									
								begin
									log_indentation_up;
									iterate (segment.ports.A.submodules, query_port'access);
									iterate (segment.ports.B.submodules, query_port'access);
									log_indentation_down;
								end query_ports_submodules;


								
								procedure query_ports_netchangers (segment : in type_net_segment) is
									use et_netlists;
									
									procedure query_port (port_cursor : in pac_netchanger_ports.cursor) is begin
										log (text => "netchanger " & et_submodules.to_string (element (port_cursor).index) &
											 " port " & et_submodules.to_string (element (port_cursor).port), level => log_threshold + 4);

										if not exists_netchanger (
											module_cursor	=> module_cursor,
											index			=> element (port_cursor).index) then -- 1, 2, 3, ...

											error;
											
											log (ERROR, "netchanger" & et_submodules.to_string (element (port_cursor).index) &
												 " does not exist !");
										end if;

										collect_netchanger_port (port => element (port_cursor), net => net_name);
									end query_port;

									
								begin
									log_indentation_up;
									iterate (segment.ports.A.netchangers, query_port'access);
									iterate (segment.ports.B.netchangers, query_port'access);
									log_indentation_down;
								end query_ports_netchangers;

								
							begin -- query_segment
								log (text => to_string (segment_cursor), level => log_threshold + 3);

								-- Check ports of devices. Issues error if device and port
								-- not found in module.devices.
								query_element (
									position	=> segment_cursor,
									process		=> query_ports_devices'access);

								-- Check ports of submodules. Issue error if submodule and port
								-- not found in module.submodules
								query_element (
									position	=> segment_cursor,
									process		=> query_ports_submodules'access);

								-- Check netchangers. Issue error if netchanger not
								-- found in module.netchangers.
								query_element (
									position	=> segment_cursor,
									process		=> query_ports_netchangers'access);
								
							end query_segment;

							
						begin -- query_segments
							iterate (strand.segments, query_segment'access);
						end query_segments;

						
					begin
						log (text => "strand " & to_string (position => element (strand_cursor).position), level => log_threshold + 2);
						log_indentation_up;
						
						query_element (
							position	=> strand_cursor,
							process		=> query_segments'access);

						log_indentation_down;
					end query_strand;

					
				begin -- query_strands
					log_indentation_up;
					iterate (net.strands, query_strand'access);
					log_indentation_down;
				end query_strands;

				
			begin
				log (text => "net " & pac_net_name.to_string (key (net_cursor)), level => log_threshold + 1);

				query_element (
					position	=> net_cursor,
					process		=> query_strands'access);
				
			end query_net;

			
		begin
			iterate (module.nets, query_net'access);
		end query_nets;


		
	begin
		log (text => "module " & to_string (module_name) & " integrity check ...", level => log_threshold);
		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		-- check nets
		query_element (
			position	=> module_cursor,
			process		=> query_nets'access);

		-- check unit positions (units sitting on top of each other)
		if not unit_positions_valid (module_cursor, log_threshold + 1) then
			error;
		end if;

		-- CS  netlist checks
		--make_netlists (
			--module_cursor	=> module_cursor,
			--write_files		=> false,
			--log_threshold	=> log_threshold + 1);

		
		if errors > 0 then
			log (WARNING, "integrity check found errors !");
			log (text => "errors   :" & natural'image (errors));
		end if;

		if warnings > 0 then
			log (WARNING, "integrity check issued warnings !");
			log (text => "warnings :" & natural'image (warnings));
		end if;

		log_indentation_down;
	end check_integrity;




	
	procedure dump_tree (
		module_name		: in pac_module_name.bounded_string;
		log_threshold	: in type_log_level) 
	is		
		module_cursor : pac_generic_modules.cursor;

		procedure query_submodules (
   			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) is
			use et_numbering;

			procedure query (cursor : in et_numbering.pac_modules.cursor) is
				use et_numbering.pac_modules;
			begin
				log (text => "instance " & to_string (element (cursor).instance) &
					 " offset " & to_string (element (cursor).device_names_offset),
					 level => log_threshold
					);
			end query;
			
		begin
			et_numbering.pac_modules.iterate (module.submod_tree, query'access);
		end query_submodules;

	begin
		log (text => "SUBMODULES TREE DUMP", level => log_threshold);
		log_indentation_up;
		
		module_cursor := locate_module (module_name);
		query_element (module_cursor, query_submodules'access);

		log_indentation_down;
	end dump_tree;




	
	procedure build_submodules_tree (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) 
	is
		-- the cursor to the given top module
		module_cursor : pac_generic_modules.cursor;
		
		submod_tree : et_numbering.pac_modules.tree := et_numbering.pac_modules.empty_tree;
		tree_cursor : et_numbering.pac_modules.cursor := et_numbering.pac_modules.root (submod_tree);

		-- A stack keeps record of the submodule level where tree_cursor is pointing at.
		package stack is new et_generic_stacks.stack_lifo (
			item	=> et_numbering.pac_modules.cursor,
			max 	=> et_submodules.nesting_depth_max);

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use et_submodules;
			submod_cursor	: et_submodules.pac_submodules.cursor := module.submods.first;
			submod_name		: pac_module_name.bounded_string; -- $ET_TEMPLATES/motor_driver
			submod_instance	: pac_module_instance_name.bounded_string; -- OSC1

			use pac_submodules;
		begin -- query_submodules in given top module
			while submod_cursor /= et_submodules.pac_submodules.no_element loop
				submod_name := to_module_name (remove_extension (to_string (element (submod_cursor).file)));
				submod_instance := key (submod_cursor);
				log (text => "submodule " & enclose_in_quotes (to_string (submod_name)) &
					 " instance " & to_string (submod_instance), level => log_threshold + 1);

				-- Before inserting the submodule in the tree, the current tree cursor
				-- at this level must be saved on the stack:
				stack.push (tree_cursor);

				et_numbering.pac_modules.insert_child (
					container	=> submod_tree,
					parent		=> tree_cursor,
					before		=> et_numbering.pac_modules.no_element,
					new_item	=> (
							name				=> submod_name,
							instance			=> submod_instance,
							device_names_offset	=> type_name_index'first
							), -- templates/CLOCK_GENERATOR OSC1 100
					position	=> tree_cursor
					);

				-- tree_cursor points now to the submodule that has been inserted last.
				-- Submodules of this submodule will be inserted as childs.
				log_indentation_up;
				
				-- locate the current submodule
				module_cursor := locate_module (submod_name);

				-- search for submodules at deeper levels. Here the procedure query_submodules
				-- calls itself (recursive).
				query_element (
					position	=> module_cursor,
					process		=> query_submodules'access);

				log_indentation_down;

				-- Restore the tree cursor. See stack.push statement above.
				tree_cursor := stack.pop;
				
				next (submod_cursor);
			end loop;
		end query_submodules;

		procedure assign_tree (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) is
		begin
			module.submod_tree := submod_tree;

			log_indentation_up;
			
			log (text => "submodules total" & 
				 count_type'image (et_numbering.pac_modules.node_count (module.submod_tree) - 1),
				 level => log_threshold + 1
				);

			log_indentation_down;
		end assign_tree;
		
	begin -- build_submodules_tree
		log (text => "module " & enclose_in_quotes (to_string (module_name)) &
			" building submodules tree ...", level => log_threshold);
		log_indentation_up;
		
		stack.init;
		
		-- locate the given top module
		module_cursor := locate_module (module_name);

		-- build the submodule tree in container submod_tree:
		query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);
		
		log_indentation_down;

		-- relocate the given top module
		module_cursor := locate_module (module_name);
		
		-- assign the submod_tree to the given top module
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> assign_tree'access);

		-- update device name offsets of submodules
		autoset_device_name_offsets (module_name, log_threshold + 1);
		
	end build_submodules_tree;




	procedure make_boms (
		module_name		: in pac_module_name.bounded_string;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_assembly_variants;
		use et_assembly_variants.pac_assembly_variants;
		use pac_assembly_variant_name;

		
		procedure make_for_variant (variant_name : in pac_assembly_variant_name.bounded_string) is

			use et_material;
			bill_of_material : pac_bom_devices.map;

			-- Collects devices of the given module and its variant in container bill_of_material.
			-- Adds to the device index the given offset.
			-- If offset is zero, we are dealing with the top module.
			procedure collect (
				module_cursor	: in pac_generic_modules.cursor;
				variant			: in pac_assembly_variant_name.bounded_string;
				offset			: in type_name_index) 
			is
				
				procedure query_devices (
					module_name	: in pac_module_name.bounded_string;
					module		: in type_generic_module) 
				is
					device_name : type_device_name;
					inserted : boolean;
					
					procedure test_inserted is begin
						if not inserted then
							log (ERROR, "multiple occurence of device " & to_string (device_name),
									console => true);
							raise constraint_error;
						end if;
					end;

					
					procedure test_partcode (partcode : in pac_device_partcode.bounded_string) is
					begin
						if pac_device_partcode.length (partcode) = 0 then
							log (WARNING, text => "device " & to_string (device_name) &
								" has no partcode !");
						end if;
					end;

					
					procedure query_properties_default (cursor_schematic : in pac_devices_sch.cursor) is 
						cursor_bom : pac_bom_devices.cursor;

						use pac_devices_sch;
						use et_assembly_variants.pac_device_variants;
						use et_symbols;
					begin
						-- the device must be real
						--if element (cursor_schematic).appearance = PCB then -- skip virtual devices
						if is_real (cursor_schematic) then -- skip virtual devices

							-- the package must be real
							if has_real_package (cursor_schematic) then

								device_name := pac_devices_sch.key (cursor_schematic);

								-- issue warning if device has no partcode
								test_partcode (element (cursor_schematic).partcode);
								
								-- Store device in bill_of_material as it is:

								apply_offset (device_name, offset, log_threshold + 2);
								
								pac_bom_devices.insert (
									container	=> bill_of_material,
									key			=> device_name, -- IC4, R3
									new_item	=> (
										value		=> element (cursor_schematic).VALUE,
										partcode	=> element (cursor_schematic).PARTCODE,
										purpose		=> element (cursor_schematic).PURPOSE,
										packge		=> get_package_model (cursor_schematic)),
									position	=> cursor_bom,
									inserted	=> inserted);
								
								test_inserted;
								
							end if;
						end if;
					end query_properties_default;

					
					procedure query_properties_variants (cursor_schematic : in pac_devices_sch.cursor) is 
						cursor_bom : pac_bom_devices.cursor;

						use pac_devices_sch;
						alt_dev_cursor : et_assembly_variants.pac_device_variants.cursor;
						use et_assembly_variants.pac_device_variants;
						use et_symbols;
						use et_device_appearance;
						use et_package_names;
					begin
						-- the device must be real
						if element (cursor_schematic).appearance = APPEARANCE_PCB then -- skip virtual devices

							-- the package must be real
							if has_real_package (cursor_schematic) then
							
								device_name := pac_devices_sch.key (cursor_schematic);
								
								-- Get a cursor to the alternative device as specified in the assembly variant:
								alt_dev_cursor := get_alternative_device (module_cursor, variant, device_name); 
								
								if alt_dev_cursor = et_assembly_variants.pac_device_variants.no_element then
								-- Device has no entry in the assembly variant. -> It is to be stored in bill_of_material as it is:

									-- issue warning if device has no partcode
									test_partcode (element (cursor_schematic).partcode);
									
									apply_offset (device_name, offset, log_threshold + 2);
									
									pac_bom_devices.insert (
										container	=> bill_of_material,
										key			=> device_name, -- IC4, R3
										new_item	=> (
											value		=> element (cursor_schematic).value,
											partcode	=> element (cursor_schematic).partcode,	
											purpose		=> element (cursor_schematic).purpose,
											packge		=> get_package_model (cursor_schematic)),
										position	=> cursor_bom,
										inserted	=> inserted);

									test_inserted;

								else
								-- Device has an entry in the assembly variant. Depending on the mounted-flag
								-- it is to be skipped or inserted in bill_of_material with alternative properties.
								-- NOTE: The package model is not affected by the assembly variant.
									case element (alt_dev_cursor).mounted is
										when NO =>
											log (text => to_string (device_name) & " not mounted -> skipped",
												level => log_threshold + 2);
											
										when YES =>
											-- issue warning if device has no partcode
											test_partcode (element (alt_dev_cursor).partcode);

											apply_offset (device_name, offset, log_threshold + 2);
											
											-- Insert the device in bill with alternative properties as defined
											-- in the assembly variant:
											pac_bom_devices.insert (
												container	=> bill_of_material,
												key			=> device_name, -- IC4, R3
												new_item	=> (
													value		=> element (alt_dev_cursor).value,
													partcode	=> element (alt_dev_cursor).partcode,
													purpose		=> element (alt_dev_cursor).purpose,
													packge		=> get_package_model (cursor_schematic)),
												position	=> cursor_bom,
												inserted	=> inserted);

											test_inserted;

											-- check partcode content
											et_conventions.validate_partcode (
												partcode		=> pac_bom_devices.element (cursor_bom).partcode,
												device_name		=> device_name,
												packge			=> to_package_name (ada.directories.base_name 
																	(to_string (pac_bom_devices.element (cursor_bom).packge))),
												value			=> pac_bom_devices.element (cursor_bom).value,
												log_threshold	=> log_threshold + 3);

									end case;
								end if;

							end if;
						end if;
					end query_properties_variants;

					
				begin -- query_devices
					-- if default variant given, then assembly variants are irrelevant:
					if is_default (variant) then

						log (text => "collecting devices from module " &
								enclose_in_quotes (to_string (module_name)) &
								" default variant by applying device index offset" & 
								to_string (offset), -- 100
							level => log_threshold + 1);
						
						log_indentation_up;
						
						pac_devices_sch.iterate (
							container	=> module.devices,
							process		=> query_properties_default'access);

					-- if a particular variant given, then collect devices accordingly:
					else
						log (text => "collecting devices from module " &
								enclose_in_quotes (to_string (module_name)) &
								" variant " & enclose_in_quotes (to_variant (variant)) &
								" by applying device index offset" & 
								to_string (offset), -- 100
							level => log_threshold + 1);
						
						log_indentation_up;
					
						pac_devices_sch.iterate (
							container	=> module.devices,
							process		=> query_properties_variants'access);

					end if;
					
					log_indentation_down;
				end query_devices;

				
			begin
				query_element (
					position	=> module_cursor,
					process		=> query_devices'access);
				
			end collect;

			
			submod_tree : et_numbering.pac_modules.tree := et_numbering.pac_modules.empty_tree;
			tree_cursor : et_numbering.pac_modules.cursor := et_numbering.pac_modules.root (submod_tree);

			
			-- A stack keeps record of the submodule level where tree_cursor is pointing at.
			package stack_level is new et_generic_stacks.stack_lifo (
				item	=> et_numbering.pac_modules.cursor,
				max 	=> et_submodules.nesting_depth_max);

			
			-- Another stack keeps record of the assembly variant on submodule levels.
			package stack_variant is new et_generic_stacks.stack_lifo (
				item	=> pac_assembly_variant_name.bounded_string,
				max 	=> et_submodules.nesting_depth_max);
			
			variant : pac_assembly_variant_name.bounded_string; -- low_cost

			
			-- Reads the submodule tree submod_tree. It is recursive, means it calls itself
			-- until the deepest submodule (the bottom of the design structure) has been reached.
			procedure query_submodules is 
				use et_numbering.pac_modules;
				module_name 	: pac_module_name.bounded_string; -- motor_driver
				parent_name 	: pac_module_name.bounded_string; -- water_pump
				module_instance	: pac_module_instance_name.bounded_string; -- MOT_DRV_3
				offset			: type_name_index;

				use et_assembly_variants.pac_submodule_variants;
				alt_submod : et_assembly_variants.pac_submodule_variants.cursor;
			begin
				log_indentation_up;

				-- start with the first submodule on the current hierarchy level
				tree_cursor := first_child (tree_cursor);

				-- iterate through the submodules on this level
				while tree_cursor /= et_numbering.pac_modules.no_element loop
					module_name := element (tree_cursor).name;
					module_instance := element (tree_cursor).instance;

					log (text => "instance " & enclose_in_quotes (to_string (module_instance)) &
						" of generic module " & enclose_in_quotes (to_string (module_name)),
						level => log_threshold + 1);

					-- In case we are on the first level, the parent module is the given top module.
					-- In that case the parent variant is the given variant of the top module.
					-- If the top module has the default variant, all submodules in all levels
					-- assume default variant too.
					if parent (tree_cursor) = root (submod_tree) then
						parent_name := make_boms.module_name;
						variant := variant_name;
					else
						parent_name := element (parent (tree_cursor)).name;
					end if;

					-- Get the device name offset of the current submodule;
					offset := element (tree_cursor).device_names_offset;

					if not is_default (variant) then
						-- Query in parent module: Is there any assembly variant specified for this submodule ?

						alt_submod := get_alternative_submodule (
									module	=> locate_module (parent_name),
									variant	=> variant,
									submod	=> module_instance);

						if alt_submod = et_assembly_variants.pac_submodule_variants.no_element then
						-- no variant specified for this submodule -> collect devices of default variant

							variant := default;
						else
						-- alternative variant specified for this submodule
							variant := element (alt_submod).variant;
						end if;

					end if;

					-- collect devices from current module
					collect (
						module_cursor	=> locate_module (module_name),
						variant			=> variant,
						offset			=> offset);

					
					if first_child (tree_cursor) = et_numbering.pac_modules.no_element then 
					-- No submodules on the current level. means we can't go deeper:
						
						log_indentation_up;
						log (text => "no submodules here -> bottom reached", level => log_threshold + 1);
						log_indentation_down;
					else
					-- There are submodules on the current level:
						
						-- backup the cursor to the current submodule on this level
						stack_level.push (tree_cursor);

						-- backup the parent assembly variant
						stack_variant.push (variant);

						-- iterate through submodules on the level below
						query_submodules; -- this is recursive !

						-- restore cursor to submodule (see stack_level.push above)
						tree_cursor := stack_level.pop;

						-- restore the parent assembly variant (see stack_variant.push above)
						variant := stack_variant.pop;
					end if;

					next_sibling (tree_cursor); -- next submodule on this level
				end loop;
				
				log_indentation_down;

				exception
					when event: others =>
						log_indentation_reset;
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;
				
			end query_submodules;

			
		begin -- make_for_variant
			if is_default (variant_name) then
				log (text => "default assembly variant ", level => log_threshold + 1);
			else
				log (text => "assembly variant " &
					enclose_in_quotes (to_string (variant_name)), level => log_threshold + 1);
			end if;

			log_indentation_up;

			-- collect devices of the given top module. the top module has no device index offset
			collect (module_cursor, variant_name, 0); 

			-- take a copy of the submodule tree of the given top module:
			submod_tree := element (module_cursor).submod_tree;

			-- set the cursor inside the tree at root position:
			tree_cursor := et_numbering.pac_modules.root (submod_tree);
			
			stack_level.init;
			stack_variant.init;

			-- collect devices of the submodules
			query_submodules;

			-- write the bom
			et_material.write_bom (
				bom				=> bill_of_material,	-- the container that holds the bom
				module_name		=> module_name,			-- motor_driver
				variant_name	=> variant_name,		-- low_cost
				--format			=> NATIVE,				-- CS should be an argument in the future
				format			=> EAGLE,				-- CS should be an argument in the future
				log_threshold	=> log_threshold + 1);
			
			log_indentation_down;
		end make_for_variant;

		
		procedure query_variant (variant_cursor : in et_assembly_variants.pac_assembly_variants.cursor) is
			use pac_assembly_variant_name;
		begin
			make_for_variant (key (variant_cursor));
		end query_variant;

		
	begin -- make_boms
		log (text => "generating BOM ...", level => log_threshold);
		log_indentation_up;

		-- locate the given top module
		module_cursor := locate_module (module_name);

		-- Build the submodule tree of the module according to the current design structure.
		-- All further operations rely on this tree:
		build_submodules_tree (
			module_name 	=> module_name,
			log_threshold	=> log_threshold + 1);

		-- make netlist of default variant
		make_for_variant (default);

		-- make netlists of other variants
		et_assembly_variants.pac_assembly_variants.iterate (element (module_cursor).variants, query_variant'access);
				
		log_indentation_down;
	end make_boms;





	-- Returns the lowest and highest device index of the given module.
	-- NOTE: This is about the indexes used by the generic module.
	function device_index_range (
		module_cursor		: in pac_generic_modules.cursor; -- the cursor to the module
		log_threshold		: in type_log_level) 
		return et_numbering.type_index_range 
	is

		index_range : et_numbering.type_index_range; -- to be returned

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module) 
		is
			use pac_devices_sch;

			device_cursor : pac_devices_sch.cursor := module.devices.first;
			index_current : type_name_index;
		begin
			while device_cursor /= pac_devices_sch.no_element loop

				index_current := get_index (key (device_cursor));
				
				if index_current < index_range.lowest then -- see specs of type_index_range for defaults
					index_range.lowest := index_current;
				end if;

				if index_current > index_range.highest then -- see specs of type_index_range for defaults
					index_range.highest := index_current;
				end if;
				
				next (device_cursor);
			end loop;

			if length (module.devices) > 0 then
				log (text => et_numbering.to_index_range (module_name, index_range),
					 level => log_threshold + 1);
			else
				log (WARNING, "no devices found in module " &
					 enclose_in_quotes (to_string (module_name)) & " !");

				index_range.lowest := type_name_index'first;
				index_range.highest := type_name_index'first;

			end if;

		end query_devices;

		
	begin -- device_index_range
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))),
			--" obtaining device index range ...",
			level => log_threshold);
		
		log_indentation_up;
		
		query_element (
			position	=> module_cursor,
			process		=> query_devices'access);

		log_indentation_down;

		return index_range;
		
	end device_index_range;

	

	

	procedure autoset_device_name_offsets (
		module_name		: in pac_module_name.bounded_string; -- the top module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor := generic_modules.first;
		index_range : et_numbering.type_index_range;

		use et_numbering;
		
		package type_ranges is new ordered_maps (
			key_type		=> pac_module_name.bounded_string, -- motor_driver (generic module name)
			"<"				=> pac_module_name."<",
			element_type	=> et_numbering.type_index_range); -- 3..190

		-- The device name indexes of all (sub)modules are stored here:
		ranges : type_ranges.map;

		
		function query_range (module : in pac_module_name.bounded_string)
		-- Returns the index range of the given generic module.
		-- NOTE: This is about the indexes used by the generic module.			
			return et_numbering.type_index_range is
			cursor : type_ranges.cursor;
		begin
			cursor := type_ranges.find (ranges, module);
			return type_ranges.element (cursor);
		end query_range;

		
		submod_tree : et_numbering.pac_modules.tree;
		tree_cursor : et_numbering.pac_modules.cursor;

		-- A stack keeps record of the submodule level where tree_cursor is pointing at.
		package stack is new et_generic_stacks.stack_lifo (
			item	=> et_numbering.pac_modules.cursor,
			max 	=> et_submodules.nesting_depth_max);

		-- For calculating the device index offset of submodule instances:
		-- The highest used device name index across the module is stored here.
		-- It increases each time a submodule is entered by procedure set_offset.
		-- The increase is the highest index used by that submodule.
		-- The next submodule will then get an offset of index_max + 1.
		index_max : type_name_index := 0;

		
		procedure increase_index_max (index : in type_name_index) is begin
			index_max := index_max + index;
		end;

		
		procedure set_offset is 
		-- Reads the submodule tree submod_tree. It is recursive, means it calls itself
		-- until the deepest submodule (the bottom of the design structure) has been reached.
			use et_numbering.pac_modules;
			module_name 	: pac_module_name.bounded_string; -- motor_driver
			parent_name 	: pac_module_name.bounded_string; -- water_pump
			module_range 	: type_index_range;
			module_instance	: pac_module_instance_name.bounded_string; -- MOT_DRV_3

			procedure assign_offset (module : in out et_numbering.type_module) is begin
			-- assign the device name offset to the current submodule according to the latest index_max.
				module.device_names_offset := index_max + 1;
				
				log (text => "module " & enclose_in_quotes (to_string (module_name)) &
					" submodule instance " & enclose_in_quotes (to_string (module_instance)) &
					" setting device names offset to" &
					to_string (module.device_names_offset),
					level => log_threshold + 2);
			end;

			
		begin -- set_offset
			log_indentation_up;

			-- start with the first submodule on the current hierarchy level
			tree_cursor := first_child (tree_cursor);

			-- iterate through the submodules on this level
			while tree_cursor /= et_numbering.pac_modules.no_element loop
				module_name := element (tree_cursor).name;
				module_instance := element (tree_cursor).instance;
				module_range := query_range (module_name);

				log (text => "instance " & enclose_in_quotes (to_string (module_instance)) &
					" of generic " & to_index_range (module_name, module_range), level => log_threshold + 1);

				-- In case we are on the first level, the parent module is the given top module.				
				if parent (tree_cursor) = root (submod_tree) then
					parent_name := autoset_device_name_offsets.module_name;
				else
					parent_name := element (parent (tree_cursor)).name;
				end if;

				-- assign the offset to the submodule
				update_element (submod_tree, tree_cursor, assign_offset'access);

				-- For the next submodule (wherever it is) the index_max must be increased the the highest
				-- index used by the current submodule:
				increase_index_max (module_range.highest);
				
				log (text => "index max" & to_string (index_max), level => log_threshold + 1);
				
				if first_child (tree_cursor) = et_numbering.pac_modules.no_element then 
					-- no submodules on the current level. means we can't go deeper.
					
					log_indentation_up;
					log (text => "no submodules here -> bottom reached", level => log_threshold + 1);
					log_indentation_down;
				else
					-- there are submodules on the current level
					
					-- backup the cursor to the current submodule on this level
					stack.push (tree_cursor);

					-- iterate through submodules on the level below
					set_offset; -- this is recursive !

					-- restore cursor to submodule (see stack.push above)
					tree_cursor := stack.pop;
				end if;

				next_sibling (tree_cursor);
			end loop;
			
			log_indentation_down;

			exception
				when event: others =>
					log_indentation_reset;
					log (text => ada.exceptions.exception_information (event), console => true);
					raise;
			
		end set_offset;


		
		procedure replace_tree (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_generic_module) 
		is begin
			module.submod_tree := submod_tree;
		end;
		

		procedure query_submodules (submod_cursor : in et_numbering.pac_modules.cursor) is
			use et_numbering.pac_modules;
			-- Map from submodule_cursor to module in et_project.modules:

			-- submod_cursor points to a submodule in the submod_tree:
			module_name	: pac_module_name.bounded_string := element (submod_cursor).name; -- motor_driver
			-- module_name now contains the generic module name like motor_driver
			
			module_cursor : pac_generic_modules.cursor := locate_module (module_name);
			-- module_cursor now points to the generic module
		begin
			-- If the range for this generic module has not been computed already, then do
			-- it now. Otherwise there is no need to do that all over:
			if not type_ranges.contains (ranges, module_name) then
				
				index_range := device_index_range (module_cursor, log_threshold + 1);

				type_ranges.insert (
					container	=> ranges,
					key			=> key (module_cursor), -- generic name
					new_item	=> index_range);
				
			end if;
		end query_submodules;

		
	begin -- autoset_device_name_offsets
		log (text => "module " & enclose_in_quotes (to_string (module_name)) &
			" exploring current ranges of device indexes ...", level => log_threshold);
		log_indentation_up;

		-- locate the given top module
		module_cursor := locate_module (module_name);

		-----------------
		-- Calculate the index range per module and store it in 
		-- container "ranges":
		-- NOTE: This is about the indexes used by the generic modules.

		-- top module:
		index_range := device_index_range (module_cursor, log_threshold + 1);

		type_ranges.insert (
			container	=> ranges,
			key			=> module_name,
			new_item	=> index_range);

		-- submodules:		
		et_numbering.pac_modules.iterate (element (module_cursor).submod_tree, query_submodules'access);
		
		-- calculation of index ranges complete
		----------------
		
		log_indentation_down;

		log (text => "autosetting device name offset of submodules instances ...", level => log_threshold);
		log_indentation_up;
		
		-- locate the given top module
-- 		module_cursor := locate_module (module_name);

		-- The first module being processed now is the given top module.
		-- Its highest used device index extends the total index range.
		increase_index_max (query_range (module_name).highest);

		-- Show the index range used by the top module:
		log (text => "top" & to_index_range (module_name, query_range (module_name)), level => log_threshold + 1);

		log (text => "index max" & to_string (index_max), level => log_threshold + 1);
		
		-- take a copy of the submodule tree of the given top module:
		submod_tree := element (module_cursor).submod_tree;

		-- set the cursor inside the tree at root position:
		tree_cursor := et_numbering.pac_modules.root (submod_tree);
		
		stack.init;

		-- start reading the submodule tree. set_offset is recursive.
		set_offset;

		-- Replace the old submodule tree by the new submod_tree. The new submod_tree now
		-- contains the device name offsets for the instantiated submodules.
		update_element (generic_modules, module_cursor, replace_tree'access);
		
		log_indentation_down;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;

	end autoset_device_name_offsets;

	
	
end et_schematic_ops.submodules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
