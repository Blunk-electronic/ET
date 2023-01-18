------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         CANVAS BOARD DEVICES                             --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
--                                                                          --
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
-- DESCRIPTION:
-- 


with et_modes.board;				use et_modes.board;
with et_canvas_board;				use et_canvas_board;
with et_board_ops.devices;			use et_board_ops.devices;
with et_device_query_board;			use et_device_query_board;


package body et_canvas_board_devices is

	use et_canvas_board.pac_canvas;
	

	procedure clear_proposed_electrical_devices is begin
		clear (proposed_electrical_device);
		selected_electrical_device := pac_proposed_electrical_devices.no_element;
	end clear_proposed_electrical_devices;

	procedure clear_proposed_non_electrical_devices is begin
		clear (proposed_non_electrical_devices);
		selected_non_electrical_device := pac_proposed_non_electrical_devices.no_element;
	end clear_proposed_non_electrical_devices;


	
	
	function collect_devices (
		module			: in pac_generic_modules.cursor;
		place			: in type_point;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_proposed_electrical_devices.list
	is
		use et_schematic;
		use pac_devices_sch;
		result : pac_proposed_electrical_devices.list;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			device_cursor : pac_devices_sch.cursor := module.devices.first;			
		begin
			while device_cursor /= pac_devices_sch.no_element loop

				log (text => "probing device " & to_string (key (device_cursor)),
					 level => log_threshold + 1);
				log_indentation_up;
					 
				if in_catch_zone (
					point_1		=> place, 
					catch_zone	=> catch_zone, 
					point_2		=> element (device_cursor).position.place) 
				then
					log_indentation_up;

					log (text => "in catch zone", level => log_threshold + 1);
					result.append ((device => device_cursor));
							
					log_indentation_down;
				end if;
				
				next (device_cursor);

				log_indentation_down;
			end loop;
		end query_devices;

		
	begin -- collect_devices		
		log (text => "looking up devices at" & to_string (place) 
			 & " catch zone" & catch_zone_to_string (catch_zone), level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_devices'access);

		log_indentation_down;
		
		return result;
	end collect_devices;



	function collect_devices (
		module			: in pac_generic_modules.cursor;
		place			: in type_point;
		catch_zone		: in type_catch_zone;
		log_threshold	: in type_log_level)
		return pac_proposed_non_electrical_devices.list
	is
		use et_schematic;
		use pac_devices_non_electric;
		result : pac_proposed_non_electrical_devices.list;

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			device_cursor : pac_devices_non_electric.cursor := module.devices_non_electric.first;			
		begin
			while device_cursor /= pac_devices_non_electric.no_element loop

				log (text => "probing device " & to_string (key (device_cursor)),
					 level => log_threshold + 1);
				log_indentation_up;
					 
				if in_catch_zone (
					point_1		=> place, 
					catch_zone	=> catch_zone, 
					point_2		=> element (device_cursor).position.place) 
				then
					log_indentation_up;

					log (text => "in catch zone", level => log_threshold + 1);
					result.append ((device => device_cursor));
							
					log_indentation_down;
				end if;
				
				next (device_cursor);

				log_indentation_down;
			end loop;
		end query_devices;

		
	begin -- collect_devices		
		log (text => "looking up devices at" & to_string (place) 
			 & " catch zone" & catch_zone_to_string (catch_zone), level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_devices'access);

		log_indentation_down;
		
		return result;
	end collect_devices;


	
	procedure clarify_electrical_device is
		use et_schematic;
		use pac_devices_sch;
		d : pac_devices_sch.cursor;
	begin
		-- On every call of this procedure we must advance from one
		-- device to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_electrical_device
		-- moves back to the start of the devices list.
		if next (selected_electrical_device) /= pac_proposed_electrical_devices.no_element then
			next (selected_electrical_device);
		else
			selected_electrical_device := proposed_electrical_device.first;
		end if;

		-- show the selected device in the status bar
		d := element (selected_electrical_device).device;
	
		set_status ("selected device " & to_string (key (d)) 
			& ". " & status_next_object_clarification);
		
	end clarify_electrical_device;

	
	procedure clarify_non_electrical_device is
		use et_schematic;
		use pac_devices_non_electric;
		d : pac_devices_non_electric.cursor;
	begin
		-- On every call of this procedure we must advance from one
		-- device to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_electrical_device
		-- moves back to the start of the devices list.
		if next (selected_non_electrical_device) /= pac_proposed_non_electrical_devices.no_element then
			next (selected_non_electrical_device);
		else
			selected_non_electrical_device := proposed_non_electrical_devices.first;
		end if;

		-- show the selected device in the status bar
		d := element (selected_non_electrical_device).device;
	
		set_status ("selected device " & to_string (key (d)) 
			& ". " & status_next_object_clarification);
		
	end clarify_non_electrical_device;


	
	procedure reset_electrical_device_move is begin
		electrical_device_move := (others => <>);
		clear_proposed_electrical_devices;
	end reset_electrical_device_move;
	
	procedure reset_non_electrical_device_move is begin
		non_electrical_device_move := (others => <>);
		clear_proposed_non_electrical_devices;
	end reset_non_electrical_device_move;
	

	
	procedure find_electrical_devices_for_move (
		point : in type_point)
	is begin
		log (text => "locating devices for move/rotate/flip ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all units in the vicinity of the given point:
		proposed_electrical_device := collect_devices (
			module			=> current_active_module,
			place			=> point,
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		
		-- evaluate the number of devices found here:
		case length (proposed_electrical_device) is
			when 0 =>
				reset_request_clarification;
				reset_electrical_device_move;
				
			when 1 =>
				electrical_device_move.being_moved := true;
				selected_electrical_device := proposed_electrical_device.first;

				case verb is
					when VERB_FLIP => 
						set_status (status_flip);

					when VERB_MOVE => 
						set_status (status_move);

					when VERB_ROTATE => 
						set_status (status_rotate);
						
					when others => null;
				end case;

				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first device
				selected_electrical_device := proposed_electrical_device.first;
		end case;
		
	end find_electrical_devices_for_move;
	

	procedure find_non_electrical_devices_for_move (
		point : in type_point)
	is begin
		log (text => "locating non-electrical devices for move/rotate/flip ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all units in the vicinity of the given point:
		proposed_non_electrical_devices := collect_devices (
			module			=> current_active_module,
			place			=> point,
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		
		-- evaluate the number of devices found here:
		case length (proposed_non_electrical_devices) is
			when 0 =>
				reset_request_clarification;
				reset_non_electrical_device_move;
				
			when 1 =>
				non_electrical_device_move.being_moved := true;
				selected_non_electrical_device := proposed_non_electrical_devices.first;

				case verb is
					when VERB_FLIP => 
						set_status (status_flip);

					when VERB_MOVE => 
						set_status (status_move);

					when VERB_ROTATE => 
						set_status (status_rotate);
						
					when others => null;
				end case;

				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first device
				selected_non_electrical_device := proposed_non_electrical_devices.first;
		end case;
		
	end find_non_electrical_devices_for_move;


	
	procedure finalize_move_electrical (
		destination		: in type_point;
		log_threshold	: in type_log_level)
	is
		sd : type_selected_electrical_device;

		use et_schematic;
		use pac_devices_sch;
	begin
		log (text => "finalizing move ...", level => log_threshold);
		log_indentation_up;

		if selected_electrical_device /= pac_proposed_electrical_devices.no_element then

			sd := element (selected_electrical_device);
			
			move_device (
				module_name		=> et_project.modules.pac_generic_modules.key (current_active_module),
				device_name		=> key (sd.device),
				coordinates		=> ABSOLUTE,
				point			=> destination,
				log_threshold	=> log_threshold);
			
		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;
		
		set_status (status_move);
		
		reset_electrical_device_move;
	end finalize_move_electrical;
	

	procedure finalize_move_non_electrical (
		destination		: in type_point;
		log_threshold	: in type_log_level)
	is
		sd : type_selected_non_electrical_device;

		use et_schematic;
		use pac_devices_non_electric;
	begin
		log (text => "finalizing move ...", level => log_threshold);
		log_indentation_up;

		if selected_non_electrical_device /= pac_proposed_non_electrical_devices.no_element then

			sd := element (selected_non_electrical_device);
			
			move_device (
				module_name		=> et_project.modules.pac_generic_modules.key (current_active_module),
				device_name		=> key (sd.device),
				coordinates		=> ABSOLUTE,
				point			=> destination,
				log_threshold	=> log_threshold);
			
		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;
		
		set_status (status_move);
		
		reset_non_electrical_device_move;
	end finalize_move_non_electrical;


	
	

	procedure finalize_rotate_electrical (
		rotation		: in type_rotation := default_rotation;
		log_threshold	: in type_log_level)
	is
		sd : type_selected_electrical_device;

		use et_schematic;
		use pac_devices_sch;
	begin
		log (text => "finalizing rotation ...", level => log_threshold);
		log_indentation_up;

		if selected_electrical_device /= pac_proposed_electrical_devices.no_element then

			sd := element (selected_electrical_device);
			
			rotate_device (
				module_name		=> et_project.modules.pac_generic_modules.key (current_active_module),
				device_name		=> key (sd.device),
				coordinates		=> RELATIVE,
				rotation		=> rotation,
				log_threshold	=> log_threshold);
			
		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;
		
		set_status (status_rotate);
		
		reset_electrical_device_move;
	end finalize_rotate_electrical;
	

	procedure finalize_rotate_non_electrical (
		rotation		: in type_rotation := default_rotation;
		log_threshold	: in type_log_level)
	is
		sd : type_selected_non_electrical_device;

		use et_schematic;
		use pac_devices_non_electric;
	begin
		log (text => "finalizing rotation ...", level => log_threshold);
		log_indentation_up;

		if selected_non_electrical_device /= pac_proposed_non_electrical_devices.no_element then

			sd := element (selected_non_electrical_device);
			
			rotate_device (
				module_name		=> et_project.modules.pac_generic_modules.key (current_active_module),
				device_name		=> key (sd.device),
				coordinates		=> RELATIVE,
				rotation		=> rotation,
				log_threshold	=> log_threshold);
			
		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;
		
		set_status (status_rotate);
		
		reset_non_electrical_device_move;
	end finalize_rotate_non_electrical;


	
	procedure finalize_flip_electrical (
		log_threshold	: in type_log_level)
	is
		sd : type_selected_electrical_device;
		face : type_face;

		use et_schematic;
		use pac_devices_sch;
	begin
		log (text => "finalizing flipping ...", level => log_threshold);
		log_indentation_up;

		if selected_electrical_device /= pac_proposed_electrical_devices.no_element then

			sd := element (selected_electrical_device);
			face := get_face (sd.device);
			toggle (face);
			
			flip_device (
				module_name		=> et_project.modules.pac_generic_modules.key (current_active_module),
				device_name		=> key (sd.device),
				face			=> face,
				log_threshold	=> log_threshold);
			
		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;
		
		set_status (status_flip);
		
		reset_electrical_device_move;
	end finalize_flip_electrical;
	

	procedure finalize_flip_non_electrical (
		log_threshold	: in type_log_level)
	is
		sd : type_selected_non_electrical_device;
		face : type_face;

		use et_schematic;
		use pac_devices_non_electric;
	begin
		log (text => "finalizing flipping ...", level => log_threshold);
		log_indentation_up;

		if selected_non_electrical_device /= pac_proposed_non_electrical_devices.no_element then

			sd := element (selected_non_electrical_device);
			face := get_face (sd.device);
			toggle (face);
			
			flip_device (
				module_name		=> et_project.modules.pac_generic_modules.key (current_active_module),
				device_name		=> key (sd.device),
				face			=> face,
				log_threshold	=> log_threshold);
			
		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;
		
		set_status (status_flip);
		
		reset_non_electrical_device_move;
	end finalize_flip_non_electrical;


	
end et_canvas_board_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
