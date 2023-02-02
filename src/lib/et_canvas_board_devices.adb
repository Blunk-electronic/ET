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


with et_canvas_board;
with et_board_ops.devices;			use et_board_ops.devices;
with et_device_query_board;			use et_device_query_board;


package body et_canvas_board_devices is

	use et_canvas_board.pac_canvas;
	

	function electrical_device_is_selected (
		d : in pac_devices_sch.cursor)
		return boolean
	is begin
		-- If there are no selected devices at all, then there is nothing to do:
		if is_empty (proposed_electrical_devices) then
			return false;
		else
			if selected_electrical_device /= pac_devices_sch.no_element then
				
				-- Compare given device and selected_electrical_device:
				if key (d) = key (selected_electrical_device) then
					return true;
				else 
					return false;
				end if;
			else
				return false;
			end if;
		end if;
	end electrical_device_is_selected;


	procedure reset_preliminary_electrical_device is begin
		preliminary_electrical_device := (others => <>);
		clear_proposed_electrical_devices;
	end reset_preliminary_electrical_device;


	
	
	function non_electrical_device_is_selected (
		d : in et_pcb.pac_devices_non_electric.cursor)
		return boolean
	is begin
		-- If there are no selected devices at all, then there is nothing to do:
		if is_empty (proposed_non_electrical_devices) then
			return false;
		else
			if selected_non_electrical_device /= pac_devices_non_electric.no_element then
				
				-- Compare then names of given device and selected_non_electrical_device:
				if key (d) = key (selected_non_electrical_device) then
					return true;
				else 
					return false;
				end if;
			else
				return false;
			end if;
		end if;
	end non_electrical_device_is_selected;


	procedure reset_preliminary_non_electrical_device is begin
		preliminary_non_electrical_device := (others => <>);
		clear_proposed_non_electrical_devices;
	end reset_preliminary_non_electrical_device;
	

	
	procedure clear_proposed_electrical_devices is begin
		clear (proposed_electrical_devices);
		selected_electrical_device := pac_devices_sch.no_element;
	end clear_proposed_electrical_devices;

	
	procedure clear_proposed_non_electrical_devices is begin
		clear (proposed_non_electrical_devices);
		selected_non_electrical_device := pac_devices_non_electric.no_element;
	end clear_proposed_non_electrical_devices;
	

	
	procedure select_electrical_device is
		use et_schematic;
	begin
		-- On every call of this procedure we must advance from one
		-- device to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_electrical_device
		-- moves back to the start of the devices list.
		if next (selected_electrical_device) /= pac_devices_sch.no_element then
			next (selected_electrical_device);
		else
			selected_electrical_device := proposed_electrical_devices.first;
		end if;

		-- show the selected device in the status bar
		set_status ("selected device " & to_string (key (selected_electrical_device)) 
			& ". " & status_next_object_clarification);
		
	end select_electrical_device;

	
	procedure select_non_electrical_device is begin
		-- On every call of this procedure we must advance from one
		-- device to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_electrical_device
		-- moves back to the start of the devices list.
		if next (selected_non_electrical_device) /= pac_devices_non_electric.no_element then
			next (selected_non_electrical_device);
		else
			selected_non_electrical_device := proposed_non_electrical_devices.first;
		end if;

		-- show the selected device in the status bar
		set_status ("selected device " & to_string (key (selected_non_electrical_device)) 
			& ". " & status_next_object_clarification);
		
	end select_non_electrical_device;

	

	
	procedure find_electrical_devices (
		point : in type_point)
	is begin
		log (text => "locating devices ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all units in the vicinity of the given point:
		proposed_electrical_devices := get_devices (
			module			=> current_active_module,
			place			=> point,
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		
		-- evaluate the number of devices found here:
		case length (proposed_electrical_devices) is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_electrical_device;
				
			when 1 =>
				preliminary_electrical_device.ready := true;
				selected_electrical_device := proposed_electrical_devices.first;
				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first device
				selected_electrical_device := proposed_electrical_devices.first;
		end case;

		log_indentation_down;
	end find_electrical_devices;
	

	procedure find_non_electrical_devices (
		point : in type_point)
	is begin
		log (text => "locating non-electrical devices ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all units in the vicinity of the given point:
		proposed_non_electrical_devices := get_devices (
			module			=> current_active_module,
			place			=> point,
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		
		-- evaluate the number of devices found here:
		case length (proposed_non_electrical_devices) is
			when 0 =>
				reset_request_clarification;
				reset_preliminary_non_electrical_device;
				
			when 1 =>
				preliminary_non_electrical_device.ready := true;
				selected_non_electrical_device := proposed_non_electrical_devices.first;
				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first device
				selected_non_electrical_device := proposed_non_electrical_devices.first;
		end case;

		log_indentation_down;
	end find_non_electrical_devices;



	
-- MOVE:

	procedure move_electrical_device (
		tool	: in type_tool;
		point	: in type_point)
	is 

		-- Assigns the final position after the move to the selected 
		-- electrical device.
		-- Resets global variable preliminary_electrical_device:
		procedure finalize is
			use et_schematic;
		begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			if selected_electrical_device /= pac_devices_sch.no_element then
				
				move_device (
					module_name		=> key (current_active_module),
					device_name		=> key (selected_electrical_device),
					coordinates		=> ABSOLUTE,
					point			=> point,
					log_threshold	=> log_threshold);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_move_device);			
			reset_preliminary_electrical_device;
		end finalize;
		
	begin
		-- Initially the preliminary_electrical_device is not ready.
		if not preliminary_electrical_device.ready then

			-- Set the tool being used:
			preliminary_electrical_device.tool := tool;
			
			if not clarification_pending then
				-- Locate all devices in the vicinity of the given point:
				find_electrical_devices (point);
				-- NOTE: If many devices have been found, then
				-- clarification is now pending.

				-- If find_electrical_devices has found only one device
				-- then the flag preliminary_electrical_device.read is set true.
				
			else
				-- Here the clarification procedure ends.
				-- A device has been selected (indicated by cursor selected_electrical_device)
				-- via procedure select_electrical_device.
				-- By setting preliminary_electrical_device.ready, the selected
				-- device will be drawn at the tool position
				-- when packages are drawn on the canvas.
				-- Furtheron, on the next call of this procedure
				-- the selected device will be assigned its final position.
				preliminary_electrical_device.ready := true;
				reset_request_clarification;
			end if;
			
		else
			-- Finally move the selected device:
			finalize;
		end if;
	end move_electrical_device;


	
	procedure move_non_electrical_device (
		tool		: in type_tool;
		point	: in type_point)
	is 
		-- Assigns the final position after the move to the selected 
		-- non-electrical device.
		-- Resets global variable preliminary_non_electrical_device:
		procedure finalize is begin
			log (text => "finalizing move ...", level => log_threshold);
			log_indentation_up;

			if selected_non_electrical_device /= pac_devices_non_electric.no_element then

				move_device (
					module_name		=> key (current_active_module),
					device_name		=> key (selected_non_electrical_device),
					coordinates		=> ABSOLUTE,
					point			=> point,
					log_threshold	=> log_threshold);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_move_device);			
			reset_preliminary_non_electrical_device;
		end finalize;
		
	begin
		-- Initially the preliminary_non_electrical_device is not ready.
		if not preliminary_non_electrical_device.ready then

			-- Set the tool being used:
			preliminary_non_electrical_device.tool := tool;
			
			if not clarification_pending then
				-- Locate all devices in the vicinity of the given point:
				find_non_electrical_devices (point);
				-- NOTE: If many devices have been found, then
				-- clarification is now pending.

				-- If find_non_electrical_devices has found only one device
				-- then the flag preliminary_non_electrical_device.read is set true.
			else
				-- Here the clarification procedure ends.
				-- A device has been selected (indicated by cursor selected_non_electrical_device)
				-- via procedure select_non_electrical_device.
				-- By setting preliminary_non_electrical_device.ready, the selected
				-- device will be drawn at the tool position
				-- when packages are drawn on the canvas.
				-- Furtheron, on the next call of this procedure
				-- the selected device will be assigned its final position.
				preliminary_non_electrical_device.ready := true;
				reset_request_clarification;
			end if;
			
		else
			-- Finally move the selected device:
			finalize;
		end if;
	end move_non_electrical_device;



	
	
-- ROTATE:

	procedure rotate_electrical_device (
		tool	: in type_tool;
		point	: in type_point)
	is 

		-- Rotates the selected electrical device by default_rotation.
		-- Resets global variable preliminary_electrical_device:
		procedure finalize is
			use et_schematic;
		begin
			log (text => "finalizing rotation ...", level => log_threshold);
			log_indentation_up;

			if selected_electrical_device /= pac_devices_sch.no_element then
				
				rotate_device (
					module_name		=> key (current_active_module),
					device_name		=> key (selected_electrical_device),
					coordinates		=> RELATIVE,
					rotation		=> default_rotation,
					log_threshold	=> log_threshold);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_rotate_device);			
			reset_preliminary_electrical_device;
		end finalize;


	begin
		-- Set the tool being used:
		preliminary_electrical_device.tool := tool;
		
		if not clarification_pending then
			-- Locate all devices in the vicinity of the given point:
			find_electrical_devices (point);
			-- NOTE: If many devices have been found, then
			-- clarification is now pending.

			-- If find_electrical_devices has found only one device
			-- then rotate that device immediately.
			if preliminary_electrical_device.ready then
				finalize;
			end if;

		else
			-- Here the clarification procedure ends.
			-- A device has been selected (indicated by cursor selected_electrical_device)
			-- via procedure select_electrical_device.
			reset_request_clarification;
			finalize;
		end if;
	end rotate_electrical_device;


	
	procedure rotate_non_electrical_device (
		tool	: in type_tool;
		point	: in type_point)
	is 

		-- Rotates the selected non-electrical device by default_rotation.
		-- Resets global variable preliminary_non_electrical_device:
		procedure finalize is begin
			log (text => "finalizing rotation ...", level => log_threshold);
			log_indentation_up;

			if selected_non_electrical_device /= pac_devices_non_electric.no_element then

				rotate_device (
					module_name		=> key (current_active_module),
					device_name		=> key (selected_non_electrical_device),
					coordinates		=> RELATIVE,
					rotation		=> default_rotation,
					log_threshold	=> log_threshold);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_rotate_device);			
			reset_preliminary_non_electrical_device;
		end finalize;

	begin
		-- Set the tool being used:
		preliminary_non_electrical_device.tool := tool;
		
		if not clarification_pending then
			-- Locate all devices in the vicinity of the given point:
			find_non_electrical_devices (point);
			-- NOTE: If many devices have been found, then
			-- clarification is now pending.

			-- If find_non_electrical_devices has found only one device
			-- then rotate that device immediately.
			if preliminary_non_electrical_device.ready then
				finalize;
			end if;

		else
			-- Here the clarification procedure ends.
			-- A device has been selected (indicated by cursor selected_non_electrical_device)
			-- via procedure select_non_electrical_device.
			reset_request_clarification;
			finalize;
		end if;
	end rotate_non_electrical_device;


	

-- FLIP / MIRROR:


	procedure flip_electrical_device (
		tool	: in type_tool;
		point	: in type_point)
	is 

		-- Flips the selected electrical device.
		-- Resets global variable preliminary_electrical_device:
		procedure finalize is
			face : type_face;
			use et_schematic;
		begin
			log (text => "finalizing flipping ...", level => log_threshold);
			log_indentation_up;

			if selected_electrical_device /= pac_devices_sch.no_element then

				face := get_face (selected_electrical_device);
				toggle (face);
				
				flip_device (
					module_name		=> key (current_active_module),
					device_name		=> key (selected_electrical_device),
					face			=> face,
					log_threshold	=> log_threshold);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_flip_device);			
			reset_preliminary_electrical_device;
		end finalize;

		
	begin
		-- Set the tool being used:
		preliminary_electrical_device.tool := tool;
		
		if not clarification_pending then
			-- Locate all devices in the vicinity of the given point:
			find_electrical_devices (point);
			-- NOTE: If many devices have been found, then
			-- clarification is now pending.

			-- If find_electrical_devices has found only one device
			-- then flip that device immediately.
			if preliminary_electrical_device.ready then
				finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- A device has been selected (indicated by cursor selected_electrical_device)
			-- via procedure select_electrical_device.
			reset_request_clarification;
			finalize;
		end if;
	end flip_electrical_device;


	
	procedure flip_non_electrical_device (
		tool	: in type_tool;
		point	: in type_point)
	is 

		procedure finalize is
			face : type_face;
		begin
			log (text => "finalizing flipping ...", level => log_threshold);
			log_indentation_up;

			if selected_non_electrical_device /= pac_devices_non_electric.no_element then

				face := get_face (selected_non_electrical_device);
				toggle (face);
				
				flip_device (
					module_name		=> key (current_active_module),
					device_name		=> key (selected_non_electrical_device),
					face			=> face,
					log_threshold	=> log_threshold);
				
			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_flip_device);			
			reset_preliminary_non_electrical_device;
		end finalize;

	begin
		-- Set the tool being used:
		preliminary_non_electrical_device.tool := tool;
		
		if not clarification_pending then
			-- Locate all devices in the vicinity of the given point:
			find_non_electrical_devices (point);
			-- NOTE: If many devices have been found, then
			-- clarification is now pending.

			-- If find_non_electrical_devices has found only one device
			-- then flip that device immediately.
			if preliminary_non_electrical_device.ready then
				finalize;
			end if;
			
		else
			-- Here the clarification procedure ends.
			-- A device has been selected (indicated by cursor selected_non_electrical_device)
			-- via procedure select_non_electrical_device.
			reset_request_clarification;
			finalize;
		end if;
	end flip_non_electrical_device;

	

-- DELETE:	
	
	procedure delete_non_electrical_device (
		tool	: in type_tool;
		point	: in type_point)
	is 

		procedure finalize is begin
			log (text => "finalizing deletion ...", level => log_threshold);
			log_indentation_up;

			if selected_non_electrical_device /= pac_devices_non_electric.no_element then

				delete_device (
					module_name		=> key (current_active_module),
					device_name		=> key (selected_non_electrical_device),
					log_threshold	=> log_threshold);

			else
				log (text => "nothing to do", level => log_threshold);
			end if;
				
			log_indentation_down;			
			set_status (status_delete_device);			
			reset_preliminary_non_electrical_device;
		end finalize;


	begin
		-- Set the tool being used:
		preliminary_non_electrical_device.tool := tool;
		
		if not clarification_pending then
			-- Locate all devices in the vicinity of the given point:
			find_non_electrical_devices (point);
			-- NOTE: If many devices have been found, then
			-- clarification is now pending.

			-- If find_non_electrical_devices has found only one device
			-- then delete that device immediately.
			if preliminary_non_electrical_device.ready then
				finalize;
			end if;

		else
			-- Here the clarification procedure ends.
			-- A device has been selected (indicated by cursor selected_non_electrical_device)
			-- via procedure select_non_electrical_device.
			reset_request_clarification;
			finalize;
		end if;
	end delete_non_electrical_device;

	
end et_canvas_board_devices;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
