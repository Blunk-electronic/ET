------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                    SCHEMATIC OPERATIONS ON SUBMODULES                    --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2024                                                -- 
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

with ada.exceptions;
with ada.directories;

with et_pcb_coordinates_2;


package body et_schematic_ops.submodules is


	function port_connected (
	-- Returns true if given port of netchanger is connected with any net.
		module	: in pac_generic_modules.cursor;
		port	: in et_netlists.type_port_netchanger)
		return boolean 
	is
		result : boolean := false; -- to be returned. goes true on the first (and only) match.

		use et_nets;
		use et_schematic;
		

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
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

					procedure query_ports (segment : in type_net_segment) is 
						use et_submodules;

						use et_netlists;
						use pac_netchanger_ports;
						port_cursor : pac_netchanger_ports.cursor := segment.ports.netchangers.first;
					begin
						while port_cursor /= pac_netchanger_ports.no_element loop
							if element (port_cursor) = port then
								result := true;
								exit; -- no more searching for netchanger ports required
							end if;
							next (port_cursor);
						end loop;
					end query_ports;
					
					
				begin -- query_segments
					while result = false and segment_cursor /= pac_net_segments.no_element loop
						
						query_element (
							position	=> segment_cursor,
							process		=> query_ports'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;

				
			begin -- query_strands
				while result = false and strand_cursor /= pac_strands.no_element loop

					query_element (
						position	=> strand_cursor,
						process		=> query_segments'access);
					
					next (strand_cursor);
				end loop;
			end query_strands;

			
		begin -- query_nets
			while result = false and net_cursor /= pac_nets.no_element loop

				pac_nets.query_element (
					position	=> net_cursor,
					process		=> query_strands'access);

				next (net_cursor);
			end loop;
			
		end query_nets;
		
	begin -- port_not_connected

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
		use et_schematic;
		
		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in type_net)
		is
			use pac_strands;
			strand_cursor : pac_strands.cursor := net.strands.first;

			procedure query_segments (strand : in type_strand) is
				use pac_net_segments;
				segment_cursor : pac_net_segments.cursor := strand.segments.first;

				procedure query_ports (segment : in type_net_segment) is 
					use et_submodules;

					use et_netlists;
					use pac_netchanger_ports;
					port_cursor : pac_netchanger_ports.cursor := segment.ports.netchangers.first;
				begin
					while port_cursor /= pac_netchanger_ports.no_element loop

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
				end query_ports;

				
			begin -- query_segments
				while result = false and segment_cursor /= pac_net_segments.no_element loop
					
					query_element (
						position	=> segment_cursor,
						process		=> query_ports'access);
					
					next (segment_cursor);
				end loop;
			end query_segments;

			
		begin -- query_strands
			while result = false and strand_cursor /= pac_strands.no_element loop

				query_element (
					position	=> strand_cursor,
					process		=> query_segments'access);
				
				next (strand_cursor);
			end loop;
		end query_strands;

		
	begin -- netchanger_as_port_available
		pac_nets.query_element (
			position	=> net,
			process		=> query_strands'access);
		
		return result;
	end netchanger_as_port_available;

	


	function exists (
	-- Returns true if the given module provides the given port.
	-- The module being searched in must be in the rig already.
		module			: in et_submodules.pac_submodules.cursor;
		port			: in pac_net_name.bounded_string; -- clock_output
		direction		: in et_submodules.type_netchanger_port_name) -- master/slave
		return boolean 
	is
		result : boolean := false; -- to be returned
		
		use et_string_processing;
		use et_submodules;
		use et_schematic;
		
		submodule_file : pac_submodule_path.bounded_string; -- $ET_TEMPLATES/motor_driver.mod
		module_name : pac_module_name.bounded_string; 
		module_cursor : pac_generic_modules.cursor;

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
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

		
	begin -- exists
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
		
	end exists;



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
		position		: in et_coordinates_2.type_position; -- the port position
		log_threshold	: in type_log_level) 
	is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			-- This flag goes true on the first match. It signals
			-- all iterations to cancel prematurely.
			port_processed : boolean := false;

			use pac_strands;
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

					
					procedure change_segment (segment : in out type_net_segment) is begin
						-- If port sits on start OR end point of segment AND if it
						-- is not already in the segment then append it to the 
						-- portlist of the segment.
						if 	segment.start_point = position.place or
							segment.end_point = position.place
						then

							-- If port not already in segment, append it.
							-- Otherwise it must not be appended again. constraint_error would arise.
							if pac_submodule_ports.contains (
								container	=> segment.ports.submodules,
								item		=> (instance, port) -- OSC1, clock_output
								) then

								log (text => " already there -> skipped", level => log_threshold + 3);
							else
								pac_submodule_ports.insert (
									container	=> segment.ports.submodules,
									new_item	=> (instance, port)); -- OSC1, clock_output

								log (text => " sits on segment -> inserted", level => log_threshold + 3);
							end if;
							
							-- signal iterations in upper levels to cancel
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
		end query_nets;

		
	begin -- insert_port
		log (text => "inserting submodule port " & enclose_in_quotes (to_string (port)) & " in net at" & 
			 to_string (position => position) & " ...", level => log_threshold);
		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module,
			process		=> query_nets'access);

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
		submodule_position : et_coordinates_2.type_position;

		-- Handling the absolute position of the port requires this variable:
		port_position : et_coordinates_2.type_position;
		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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
				if exists (
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
			offset		=> to_distance_relative (submodule_position.place));

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
		position		: in et_coordinates_2.type_position; -- the submodule position (only sheet matters)
		log_threshold	: in type_log_level) 
	is

		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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
						use pac_submodule_ports;
						port_cursor : pac_submodule_ports.cursor;
					begin
						-- Search for the port and delete it if existing:
						port_cursor := find (
							container	=> segment.ports.submodules,
							item		=> port); -- OSC1, clock_output

						if port_cursor /= pac_submodule_ports.no_element then
							delete (segment.ports.submodules, port_cursor);
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
		submodule_position : et_coordinates_2.type_position;
		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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
		submodule_position : et_coordinates_2.type_position;

		-- Handling the absolute position of the port requires this variable:
		port_position : et_coordinates_2.type_position;
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified


		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
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
							mirror (submod_pos_tmp, X);
							mirror (submod_pos_tmp, Y);

							-- Subtract from given point the absolute submodule position:
							move_by (
								point	=> point_tmp,
								offset	=> to_distance_relative (submod_pos_tmp));

							-- assign the new port position
							port.position := point_tmp;

						when RELATIVE =>
							move_by (
								point	=> port.position,
								offset	=> to_distance_relative (point));
							
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
							offset	=> to_distance_relative (submodule_position.place));
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
		point 			: in et_coordinates_2.type_position;
		log_threshold	: in type_log_level) 
	is 
		ports : type_ports;
		port : type_submodule_port;

		use pac_submodule_ports;
		use pac_device_ports;

		use et_netlists;
		use pac_netchanger_ports;
		
	begin -- movable_test
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
			ports := ports_at_place (module_cursor, point, log_threshold + 1);

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
			module		: in out type_module) is

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
						if segment.start_point = port_before then
							log (text => "move segment start point from" & 
								to_string (segment.start_point),
								level => log_threshold + 3);

							segment.start_point := port_after;

							log (text => "to" & 
								to_string (segment.start_point),
								level => log_threshold + 3);

							-- signal iterations in upper level to cancel
							drag_processed := true;
						end if;

						
						-- if port sits on an end point of a segment -> move end point
						if segment.end_point = port_before then
							log (text => "move segment end point from" & 
								to_string (segment.end_point),
								level => log_threshold + 3);

							segment.end_point := port_after;

							log (text => "to" & 
								to_string (segment.end_point),
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
			 to_sheet (sheet) & " ...", level => log_threshold);
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
		pos_before		: in et_coordinates_2.type_position;	-- the old port position
		pos_after		: in et_coordinates_2.type_position;	-- the new port position
		log_threshold	: in type_log_level) 
	is
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
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
						if segment.start_point = pos_before.place then
							log (text => "move segment start point from" & 
								to_string (segment.start_point),
								level => log_threshold + 3);

							segment.start_point := pos_after.place;

							log (text => "to" & 
								to_string (segment.start_point),
								level => log_threshold + 3);

							-- signal iterations in upper level to cancel
							drag_processed := true;
						end if;

						-- if port sits on an end point of a segment -> move end point
						if segment.end_point = pos_before.place then
							log (text => "move segment end point from" & 
								to_string (segment.end_point),
								level => log_threshold + 3);

							segment.end_point := pos_after.place;

							log (text => "to" & 
								to_string (segment.end_point),
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
			 to_sheet (get_sheet (pos_before)) & " ...", level => log_threshold);
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
		submodule_position : et_coordinates_2.type_position;

		-- Handling the absolute position of the port requires these variables:
		port_position_before : et_coordinates_2.type_position;
		port_position_after  : et_coordinates_2.type_position;
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
	
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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
						offset	=> to_distance_relative (submodule_position.place));
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
							mirror (submod_pos_tmp, X);
							mirror (submod_pos_tmp, Y);

							-- Subtract from given point the absolute submodule position:
							move_by (
								point	=> point_tmp,
								offset	=> to_distance_relative (submod_pos_tmp));

							-- assign the new port position
							port.position := point_tmp;

						when RELATIVE =>
							move_by (
								point	=> port.position,
								offset	=> to_distance_relative (point));
							
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
							offset	=> to_distance_relative (submodule_position.place));
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
			module		: in out type_module) 
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
							-- If port sits on start OR end point of segment AND if it
							-- is not already in the segment then append it to the 
							-- portlist of the segment.
							if 	segment.start_point = port or
								segment.end_point = port then

								-- If port not already in segment, append it.
								-- Otherwise it must not be appended again. constraint_error would arise.
								if pac_netchanger_ports.contains (
									container	=> segment.ports.netchangers,
									item		=> (index, name)
									) then

									log (text => " already there -> skipped", level => log_threshold + 5);
								else
									pac_netchanger_ports.insert (
										container	=> segment.ports.netchangers,
										new_item	=> (index, name)); -- 1,2,3, .. / master/slave

									log (text => " sits on segment -> inserted", level => log_threshold + 5);
								end if;
								
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

		
	begin --insert_ports
		log (text => "inserting netchanger ports in nets on sheet" & 
			 to_sheet (sheet) & " ...", level => log_threshold);
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
			module		: in type_module)
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

	


	
	
	procedure add_netchanger (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		place			: in et_coordinates_2.type_position; -- sheet/x/y/rotation
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		
		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
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
			location 			: in et_coordinates_2.type_position; -- only sheet number matters
			netchanger_ports	: in et_submodules.type_netchanger_ports) -- x/y of master and slave port
		is			

			
			procedure test_point (
				point		: in et_coordinates_2.type_position; -- sheet/x/y -- the point to be probed
				port_name	: in et_submodules.type_netchanger_port_name) -- master/slave
			is 
				use et_netlists;
				ports : type_ports;
				port : type_port_netchanger;

				use et_nets.pac_submodule_ports;
				use pac_device_ports;
				use pac_netchanger_ports;
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
					ports := ports_at_place (module_cursor, point, log_threshold + 2);

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
			module		: in out type_module) 
		is
			cursor : pac_netchangers.cursor;
			location : et_coordinates_2.type_position;
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
							offset		=> to_distance_relative (point));
				end case;

				-- move the netchanger to the new position
				update_element (
					container	=> module.netchangers,
					position	=> cursor,
					process		=> move'access);

				-- Get the NEW absolute positions of the netchanger ports AFTER
				-- the move operation according to location and rotation in schematic.
				ports_new := netchanger_ports (cursor);

				-- Change net segments in the affected nets (type_module.nets):
				drag_net_segments (
					module			=> module_cursor,
					ports_before	=> ports_old,
					ports_after		=> ports_new,
					sheet			=> get_sheet (location),
					log_threshold	=> log_threshold + 1);

				-- The drag operation might result in new port-to-net connections.
				-- So we must insert new ports in segments.
				-- Insert possible new netchanger ports in the nets (type_module.nets):
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
			module		: in out type_module) 
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
						use et_netlists;
						use pac_netchanger_ports;
						use et_submodules;
						port_cursor : pac_netchanger_ports.cursor;

						
						procedure delete_port is begin
							log (text => "sheet" & to_sheet (sheet) & " net " &
								to_string (key (net_cursor)) & " " &
								to_string (segment_cursor),
								level => log_threshold + 1);
							delete (segment.ports.netchangers, port_cursor);
						end;

						
					begin -- query_ports
						-- Search for the master port if it has not been deleted yet:
						if not deleted_ports.master then
							port_cursor := find (segment.ports.netchangers, (index, MASTER));
							if port_cursor /= pac_netchanger_ports.no_element then
								delete_port;
								deleted_ports.master := true;
							end if;
						end if;

						-- Search for the slave port if it has not been deleted yet:
						if not deleted_ports.slave then
							port_cursor := find (segment.ports.netchangers, (index, SLAVE));
							if port_cursor /= pac_netchanger_ports.no_element then
								delete_port;
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
			module		: in out type_module) 
		is
			cursor : pac_netchangers.cursor;
			location : et_coordinates_2.type_position;
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
				location := position (
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
					" to sheet" & to_sheet (sheet) &
					to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving netchanger" & to_string (index) &
					" by " & to_sheet_relative (sheet) & " sheet(s)" &
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
		rotation		: in et_coordinates_2.type_rotation_model; -- 90
		log_threshold	: in type_log_level) 
	is
		use et_submodules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_netchangers (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			cursor : pac_netchangers.cursor;
			location : et_coordinates_2.type_position;
			rotation : et_coordinates_2.type_rotation_model;
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
			module		: in out type_module)
		is
			cursor : pac_netchangers.cursor;
			location : et_coordinates_2.type_position;

			use pac_netchangers;
		begin

			-- locate given netchanger
			cursor := find (module.netchangers, index);

			if cursor /= pac_netchangers.no_element then 
				-- netchanger exists

				-- Get coordinates of netchanger master port.
				-- Since the ports of a netchanger are all on the same sheet,
				-- the sheet is now provided by location.
				location := position (
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
		position		: in et_coordinates_2.type_position; -- sheet, lower left corner x/y 
		size			: in et_submodules.type_submodule_size; -- the size of the box in x and y
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		full_file_name : constant string := expand (et_submodules.to_string (file));
		
		use et_submodules;

		
		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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
		-- Read the submodule file and store its content in container et_project.modules:
		et_project.modules.read_module (to_string (file), log_threshold + 1);		

	end add_submodule;




	-- Deletes all references to the given submodule in the nets.
	procedure delete_ports (
		module_cursor	: in pac_generic_modules.cursor;					-- the module
		instance		: in pac_module_instance_name.bounded_string; -- the submodule instance
		position		: in et_coordinates_2.type_position; 		-- the location in the schematic (only sheet matters)
		log_threshold	: in type_log_level)
	is
		
		-- Removes all references to the submodule instance from the net segments.
		procedure query_nets (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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

					procedure change_segment (segment : in out type_net_segment) is
						use pac_module_instance_name;
						use pac_submodule_ports;
						port_cursor : pac_submodule_ports.cursor := segment.ports.submodules.first;
					begin
						while port_cursor /= pac_submodule_ports.no_element loop
							if element (port_cursor).module_name = instance then -- OSC1
								delete (segment.ports.submodules, port_cursor);
							end if;
							next (port_cursor);
						end loop;
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
		submodule_position : et_coordinates_2.type_position;
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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
		submodule_position_before : et_coordinates_2.type_position;
		submodule_position_after : et_coordinates_2.type_position;		
		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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
				position : et_coordinates_2.type_position;
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
					" to sheet" & to_sheet (sheet) &
					to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving submodule instance " & to_string (instance) &
					" by " & to_sheet_relative (sheet) & " sheet(s)" &
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
			before	: et_coordinates_2.type_position;
			after 	: et_coordinates_2.type_position;
		end record;

		-- Since there are lots of submodule ports we store the drag points in a simple list:
		package type_drags is new doubly_linked_lists (type_drag);
		drag_list : type_drags.list;

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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
						offset	=> to_distance_relative (port.position));
			
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
								offset	=> to_distance_relative (point));

					end case;

					move_by (
						point	=> drag.after.place,
						offset	=> to_distance_relative (port.position));

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
							offset	=> to_distance_relative (point));
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
		destination		: in et_coordinates_2.type_position; -- sheet/x/y
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified
		
		use et_submodules;

		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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
				position : et_coordinates_2.type_position;
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
				submodule.position_in_board := et_pcb_coordinates_2.pac_geometry_2.origin_zero_rotation;

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
			" at" & et_coordinates_2.to_string (position => destination), level => log_threshold);

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
			module		: in out type_module) 
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
				position : et_coordinates_2.type_position;
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
		if exists (module_cursor, instance_new) then
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



	
	procedure set_submodule_file (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		file			: in et_submodules.pac_submodule_path.bounded_string; -- the file name of the submodule like templates/oscillator.mod
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		full_file_name : constant string := expand (et_submodules.to_string (file));
		
		use et_submodules;

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
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

					if not exists (
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

	
	
	
end et_schematic_ops.submodules;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
