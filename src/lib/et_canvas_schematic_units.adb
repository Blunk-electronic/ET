------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        CANVAS SCHEMATIC UNITS                            --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2020 Mario Blunk, Blunk electronic          --
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

with ada.text_io;					use ada.text_io;

with gtkada.file_selection;
with gtk.menu;
with gtk.menu_item;
with gtk.menu_shell;

with et_general;					use et_general;
with et_geometry;					use et_geometry;
with et_devices;					use et_devices;
with et_device_rw;
with et_packages;
with et_schematic;					use et_schematic;
with et_modes.schematic;

with et_canvas_schematic;			use et_canvas_schematic;

package body et_canvas_schematic_units is

	use et_canvas_schematic.pac_canvas;

	procedure clear_proposed_units is begin
		clear (proposed_units);
		selected_unit := pac_proposed_units.no_element;
	end clear_proposed_units;
		
	function collect_units (
		module			: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position; -- sheet/x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		log_threshold	: in type_log_level)
		return pac_proposed_units.list
	is
		result : pac_proposed_units.list;

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) 
		is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor := module.devices.first;

			procedure query_units (
				device_name	: in type_name;
				device		: in et_schematic.type_device)
			is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor := device.units.first;
			begin
				while unit_cursor /= et_schematic.type_units.no_element loop
					
					-- We are interested in units on the given sheet only:
					if sheet (element (unit_cursor).position) = sheet (place) then

						log (text => "probing unit " & to_string (unit_cursor),
							level => log_threshold + 1);

						if in_catch_zone (place, catch_zone, element (unit_cursor).position) then
							log_indentation_up;

							log (text => "in catch zone", level => log_threshold + 1);
							result.append ((device_cursor, unit_cursor));
							
							log_indentation_down;
						end if;
					end if;

					next (unit_cursor);
				end loop;
				
			end query_units;
			
		begin -- query_devices
			while device_cursor /= et_schematic.type_devices.no_element loop

				log (text => "probing device " & to_string (key (device_cursor)),
					 level => log_threshold + 1);
				log_indentation_up;
					 
				query_element (
					position	=> device_cursor,
					process		=> query_units'access);

				next (device_cursor);

				log_indentation_down;
			end loop;
		end query_devices;

	begin -- collect_units
		log (text => "looking up units at" & to_string (place) 
			 & " catch zone" & to_string (catch_zone), level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_devices'access);

		log_indentation_down;
		
		return result;
		
	end collect_units;


	procedure clarify_unit is
		use et_schematic;
		use et_schematic_ops.units;
		u : type_units.cursor;
	begin
		-- On every call of this procedure we must advance from one
		-- unit to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_unit
		-- moves back to the start of the unit list.
		if next (selected_unit) /= pac_proposed_units.no_element then
			next (selected_unit);
		else
			selected_unit := proposed_units.first;
		end if;

		-- show the selected unit in the status bar
		u := element (selected_unit).unit;
	
		set_status ("selected unit " & to_string (u) 
			& ". " & status_next_object_clarification);
		
	end clarify_unit;


	
-- DELETE UNIT

	-- Deletes a unit of a device. 
	-- In case the last unit has been deleted, then the device is 
	-- deleted entirely from the module.
	-- Mind that the parameter unit is an in/out !
	procedure delete_unit (
		module_cursor	: in pac_generic_modules.cursor;
		unit			: in out type_selected_unit;
		log_threshold	: in type_log_level)
	is
		use et_schematic.type_devices;
		use et_schematic.type_units;

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			
			-- temporarily storage of unit coordinates.
			-- There will be only one unit in this container.
			position_of_unit : type_unit_positions.map;

			ports : et_symbols.type_ports.map;

			procedure query_units (
				device_name	: in type_name;
				device		: in out et_schematic.type_device) is
			begin
				-- Load the single unit position and insert in container "position_of_unit"
				type_unit_positions.insert (
					container	=> position_of_unit, 
					key			=> key (unit.unit),
					new_item	=> element (unit.unit).position);

				log_unit_positions (position_of_unit, log_threshold + 1);
				
				-- delete the unit
				delete (device.units, key (unit.unit));
				
			end query_units;
			
			units_invoked : boolean := true; -- goes false if no unit used anymore

			procedure query_number_of_invoked_units (
				device_name	: in type_name;
				device		: in et_schematic.type_device) is
			begin
				if length (device.units) = 0 then
					units_invoked := false;
				end if;
			end query_number_of_invoked_units;

		begin -- query_devices
			-- Fetch the ports of the unit to be deleted.
			ports := ports_of_unit (unit.device, key (unit.unit));

			-- locate the unit, load position and then delete the targeted unit
			update_element (
				container	=> module.devices,
				position	=> unit.device,
				process		=> query_units'access);
			
			-- Delete the ports of the targeted unit from module.nets
			delete_ports (
				module			=> module_cursor,
				device			=> key (unit.device),
				ports			=> ports,
				sheets			=> position_of_unit, -- there is only one unit -> only one sheet to look at
				log_threshold	=> log_threshold + 1);

			-- In case no more units are invoked then the device must be
			-- deleted entirely from module.devices.
			-- First we query the number of still invoked units. If none invoked,
			-- the flag units_invoked goes false.
			query_element (
				position	=> unit.device,
				process		=> query_number_of_invoked_units'access);

			if not units_invoked then
				delete (module.devices, unit.device);
			end if;

		end query_devices;

	begin -- delete_unit
		log (text => "module " & to_string (key (module_cursor)) &
			 " deleting " & to_string (key (unit.device)) & " unit " & 
			 to_string (key (unit.unit)) & " ...", level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);
		
		log_indentation_down;				
	end delete_unit;
	
	procedure finalize_delete (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		unit			: in type_selected_unit; -- device/unit
		log_threshold	: in type_log_level)
	is 
		su : type_selected_unit := unit;		
	begin
		delete_unit (module_cursor, su, log_threshold);
		-- NOTE: su has been modified !

		reset_request_clarification;
		
		set_status (status_delete);
		
		clear_proposed_units;
	end finalize_delete;
	
	procedure delete_unit (point : in type_point) is 
		use et_schematic_ops.units;
		unit_cursor : pac_proposed_units.cursor;
	begin
		log (text => "deleting unit ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all units in the vicinity of the given point:
		proposed_units := collect_units (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of units found here:
		case length (proposed_units) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				unit_cursor := proposed_units.first;
			
				finalize_delete (
					module_cursor	=> current_active_module,
					unit			=> element (unit_cursor),
					log_threshold	=> log_threshold + 1);

			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first unit
				selected_unit := proposed_units.first;
		end case;
		
		log_indentation_down;
	end delete_unit;

	procedure delete_selected_unit is
		use et_schematic_ops.units;
	begin
		log (text => "deleting unit after clarification ...", level => log_threshold);
		log_indentation_up;

		finalize_delete (
			module_cursor	=> current_active_module,
			unit			=> element (selected_unit),
			log_threshold	=> log_threshold + 1);
		
		log_indentation_down;
	end delete_selected_unit;
	

-- MOVE/DRAG/ROTATE UNIT

	procedure reset_unit is begin
		unit := (others => <>);
		clear_proposed_units;
	end reset_unit;

	procedure finalize_move (
		destination		: in type_point;
		log_threshold	: in type_log_level)
	is
		su : type_selected_unit;

		use et_schematic.type_devices;
		use et_schematic.type_units;
	begin
		log (text => "finalizing move ...", level => log_threshold);
		log_indentation_up;

		if selected_unit /= pac_proposed_units.no_element then

			su := element (selected_unit);
			
			move_unit (
				module_name		=> et_project.modules.pac_generic_modules.key (current_active_module),
				device_name		=> key (su.device),
				unit_name		=> key (su.unit),
				coordinates		=> ABSOLUTE,
				sheet			=> current_active_sheet,
				point			=> destination,
				log_threshold	=> log_threshold);

			-- CS write a reduced procedure of move_unit that takes a 
			-- module cursor, device cursor and unit cursor instead.
			
		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;

		set_status (status_move);
		
		reset_unit;
	end finalize_move;

	procedure reset_segments_being_dragged is begin
		segments_being_dragged.clear;
	end reset_segments_being_dragged;
	
	procedure finalize_drag (
		destination		: in type_point;
		log_threshold	: in type_log_level)
	is
		su : type_selected_unit;

		use et_schematic.type_devices;
		use et_schematic.type_units;
	begin
		log (text => "finalizing drag ...", level => log_threshold);
		log_indentation_up;

		if selected_unit /= pac_proposed_units.no_element then

			su := element (selected_unit);
			
			drag_unit (
				module_name		=> et_project.modules.pac_generic_modules.key (current_active_module),
				device_name		=> key (su.device),
				unit_name		=> key (su.unit),
				coordinates		=> ABSOLUTE,
				point			=> destination,
				log_threshold	=> log_threshold);

		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;

		set_status (status_drag);

		reset_segments_being_dragged;
		
		reset_unit;
	end finalize_drag;
	
	
	procedure find_units (point : in type_point) is 
		use et_modes.schematic;
	begin
		log (text => "locating units ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all units in the vicinity of the given point:
		proposed_units := collect_units (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of units found here:
		case length (proposed_units) is
			when 0 =>
				reset_request_clarification;
				reset_unit;
				
			when 1 =>
				unit.being_moved := true;
				selected_unit := proposed_units.first;

				case verb is
					when VERB_DRAG => 
						find_attached_segments;
						set_status (status_drag);
						
					when VERB_MOVE => 
						set_status (status_move);
						
					when others => null;
				end case;

				reset_request_clarification;
				
			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first unit
				selected_unit := proposed_units.first;
		end case;
		
		log_indentation_down;
	end find_units;

	procedure find_attached_segments is
		-- Device and unit name of the selected unit:
		use et_schematic.type_devices;
		use et_schematic.type_units;
		su : type_selected_unit := element (selected_unit);
		device_name : constant type_name := key (su.device);
		unit_name : constant type_unit_name.bounded_string := key (su.unit);

		-- The ports with their positions the selected unit:
		ports : et_symbols.type_ports.map;

		-- The initial position of the selected unit before 
		-- the drag:
		unit_position : et_coordinates.type_position;
		
		procedure get_ports (su : in type_selected_unit) is 
			use et_schematic.type_units;
		begin
			-- Get the unit position before the drag:
			unit_position := element (su.unit).position;

			-- Get the default port positions as defined in the library:
			ports := ports_of_unit (su.device, key (su.unit));

			-- Calculate the port positions in the schematic before the drag:
			rotate_ports (ports, rot (unit_position));
			move_ports (ports, unit_position);
		end get_ports;

		
		procedure query_port (p : in et_symbols.type_ports.cursor) is
			use et_schematic.type_nets;
			use et_symbols.type_ports;

			procedure query_net (n : in type_nets.cursor) is
				use et_schematic.type_strands;

				procedure query_strand (s : in type_strands.cursor) is
					use et_schematic.type_net_segments;

					procedure query_segment (g : in type_net_segments.cursor) is
						use et_schematic.pac_shapes;
					begin
						if element (g).start_point = element (p).position then
							segments_being_dragged.append ((
-- 								net		=> n,
-- 								strand	=> s,
								segment	=> element (g),
								zone	=> START_POINT));

							--log (text => "dg1" & to_string (element (g)), console => true);
						end if;

						if element (g).end_point = element (p).position then
							segments_being_dragged.append ((
-- 								net		=> n,
-- 								strand	=> s,
								segment	=> element (g),
								zone	=> END_POINT));

							--log (text => "dg1" & to_string (element (g)), console => true);
						end if;
					end query_segment;
					
				begin -- query_strand
					if sheet (element (s).position) = current_active_sheet then
						iterate (element (s).segments, query_segment'access);
					end if;
				end query_strand;
				
			begin
				iterate (element (n).strands, query_strand'access);
			end query_net;
			
		begin
			iterate (element (current_active_module).nets, query_net'access);
		end query_port;
		
	begin -- find_attached_segments
		--log (text => "find attached segments", console => true);
		
		query_element (selected_unit, get_ports'access);
		-- now the ports of the selected unit are in "ports"
		
		-- Test whether the ports of the unit can be dragged.
		-- CS: Might become obsolete once ports at the same x/y position are prevented.
		if movable (current_active_module, device_name, unit_name, 
			unit_position, ports, log_threshold + 10)
			-- CS: level 10 avoids excessive log information. find a more elegant way.
		then
			et_symbols.type_ports.iterate (ports, query_port'access);
		else
			null;
		end if;
		
	end find_attached_segments;
	
	-- Rotates a unit of a device by 90 degree clockwise. 
	-- Disconnects the unit from attached net segments before the rotation.
	-- Connects the unit with net segments after the rotation.
	-- Rotates the placeholders about the unit center.
	-- Mind that the parameter unit is an in/out !
	procedure rotate_unit (
		module_cursor	: in pac_generic_modules.cursor;
		unit			: in out type_selected_unit;
		log_threshold	: in type_log_level)
	is
		rotation : constant et_coordinates.type_rotation := 90.0;

		use et_schematic.type_devices;
		use et_schematic.type_units;

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			position_of_unit : et_coordinates.type_position;
			rotation_before : et_coordinates.type_rotation;

			ports_lib, ports_scratch : et_symbols.type_ports.map;

			procedure query_units (
				device_name	: in type_name;
				device		: in out et_schematic.type_device)
			is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;

				procedure rotate_unit (
					name	: in type_unit_name.bounded_string; -- A
					unit	: in out et_schematic.type_unit) is

					preamble : constant string := " placeholder now at";

					procedure rotate_placeholders_relative (rot : in type_rotation) is 
						use et_symbols.pac_text;
					begin
						-- Rotate position of placeholders around the unit origin. 
					
						-- NAME
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.name.rotation := snap (unit.name.rotation + rot);

						-- rotate the placeholder anchor point around the symbol origin:
						rotate_by (unit.name.position, rot);

						log (text => "name" & preamble & to_string (unit.name.position), 
								level => log_threshold + 2);


						-- VALUE
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.value.rotation := snap (unit.value.rotation + rot);

						-- rotate the placeholder anchor point around the symbol origin:
						rotate_by (unit.value.position, rot);

						log (text => "value" & preamble & to_string (unit.value.position), 
								level => log_threshold + 2);


						-- PURPOSE
						-- Rotate the placeholder around its own anchor point so that it
						-- it is readable from the front or from the right.
						unit.purpose.rotation := snap (unit.purpose.rotation + rot);

						-- rotate the placeholder anchor point around the symbol origin:
						rotate_by (unit.purpose.position, rot);

						log (text => "purpose" & preamble & to_string (unit.purpose.position), 
								level => log_threshold + 2);

					end rotate_placeholders_relative;

				begin -- rotate_unit
					set (unit.position, add (rotation_before, rotation));
					
					log (text => "rotation now" & to_string (rot (unit.position)),
							level => log_threshold + 1);

					rotate_placeholders_relative (rotation);
				end rotate_unit;
				
			begin -- query_units

				-- load unit position and current rotation
				position_of_unit := element (unit.unit).position;
				rotation_before := rot (element (unit.unit).position);

				-- log unit position and current rotation
				log (text => to_string (position => position_of_unit) &
					" rotation before" & to_string (rotation_before),
					level => log_threshold + 1);

				log_indentation_up;
				
				type_units.update_element (
					container	=> device.units,
					position	=> unit.unit,
					process		=> rotate_unit'access);

				log_indentation_down;
			end query_units;

		begin -- query_devices

			-- rotate the unit
			update_element (
				container	=> module.devices,
				position	=> unit.device,
				process		=> query_units'access);
			
			log_indentation_up;

			-- Fetch the ports of the unit to be rotated.
			-- The coordinates here are the default positions (in the library model)
			-- relative to the center of the units.
			ports_lib := ports_of_unit (unit.device, type_units.key (unit.unit));
			
			ports_scratch := ports_lib;						 

			-- Calculate the absolute positions of the unit ports in the schematic
			-- as they are BEFORE the rotation:
			rotate_ports (ports_scratch, rotation_before);
			move_ports (ports_scratch, position_of_unit);
			
			-- Delete the old ports of the targeted unit from module.nets.
			-- The unit is on a certain sheet. The procedure delete_ports however
			-- requires a list of unit positions (containing sheet numbers).
			-- So we create a list "sheets", put the unit name and position in it,
			-- and pass it to procedure delete_ports:
			declare
				sheets : type_unit_positions.map;
			begin
				type_unit_positions.insert (
					container	=> sheets,
					key			=> key (unit.unit),
					new_item	=> position_of_unit);

				delete_ports (
					module			=> module_cursor,
					device			=> key (unit.device),
					ports			=> ports_scratch,
					sheets			=> sheets, 
					log_threshold	=> log_threshold + 1);
			end;

			-- Calculate the new positions of the unit ports.
			-- The fixed angle of rotation adds to the rotation_before:
			rotate_ports (ports_lib, add (rotation_before, rotation));
			
			move_ports (ports_lib, position_of_unit);
			
			-- Insert the new unit ports in the nets (type_module.nets):
			insert_ports (
				module			=> module_cursor,
				device			=> key (unit.device),
				unit			=> key (unit.unit),
				ports			=> ports_lib,
				sheet			=> et_coordinates.sheet (position_of_unit),
				log_threshold	=> log_threshold + 1);
			
			log_indentation_down;				

		end query_devices;
		
	begin -- rotate_unit
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) 
			 & " rotating " & to_string (key (unit.device)) & " unit " 
			 & to_string (key (unit.unit)) 
			 & " by " & to_string (rotation) & " degree ...",
			 level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);
		
		log_indentation_down;				
	end rotate_unit;
	
	procedure finalize_rotate_unit (
		module_cursor	: in pac_generic_modules.cursor; -- motor_driver
		unit			: in type_selected_unit; -- device/unit
		log_threshold	: in type_log_level)
	is 
		su : type_selected_unit := unit;		
	begin
		rotate_unit (module_cursor, su, log_threshold);
		-- NOTE: su has been modified !

		reset_request_clarification;
		
		set_status (status_rotate);
		
		clear_proposed_units;
	end finalize_rotate_unit;
	
	procedure rotate_unit (point : in type_point) is 
		use et_schematic_ops.units;
		unit_cursor : pac_proposed_units.cursor;
	begin
		log (text => "rotating unit ...", level => log_threshold);
		log_indentation_up;
		
		-- Collect all units in the vicinity of the given point:
		proposed_units := collect_units (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of units found here:
		case length (proposed_units) is
			when 0 =>
				reset_request_clarification;
				
			when 1 =>
				unit_cursor := proposed_units.first;
			
				finalize_rotate_unit (
					module_cursor	=> current_active_module,
					unit			=> element (unit_cursor),
					log_threshold	=> log_threshold + 1);

			when others =>
				--log (text => "many objects", level => log_threshold + 2);
				set_request_clarification;

				-- preselect the first unit
				selected_unit := proposed_units.first;
		end case;
		
		log_indentation_down;
	end rotate_unit;

	procedure rotate_selected_unit is
		use et_schematic_ops.units;
	begin
		log (text => "rotating unit after clarification ...", level => log_threshold);
		log_indentation_up;

		finalize_rotate_unit (
			module_cursor	=> current_active_module,
			unit			=> element (selected_unit),
			log_threshold	=> log_threshold + 1);
		
		log_indentation_down;
	end rotate_selected_unit;


-- ADD UNIT/DEVICE
	--use gtk.menu_shell;
	
	procedure variant_selected (self : access gtk.menu_item.gtk_menu_item_record'class) is

		-- Extract the variant name from field 3 of the menu item:
		var_name : constant string := get_field_from_line (
			text_in		=> self.get_label,
			position	=> 3);

	begin
		unit_add.variant := to_name (var_name);
		
		put_line (var_name & " selected");
	end variant_selected;
	
	procedure add_device is
		use gtkada.file_selection;
		use gtk.menu;
		use gtk.menu_item;
		use et_device_rw;
		use et_packages;

		device_model : type_device_model_file.bounded_string;

		use et_devices.type_devices;
		device_cursor_lib : et_devices.type_devices.cursor; -- points to the device in the library

		use pac_variants;
		variants : pac_variants.map;
		
		procedure show_variants_menu is
			m : gtk_menu;
			i : gtk_menu_item;
			
			procedure query_variant (c : in pac_variants.cursor) is
				package_model : constant string := to_string (element (c).package_model);
			begin
				-- Build the menu item. NOTE: The actual variant name must be
				-- the 3rd string of the entry. Procedure variant_selected expects
				-- it at this place:
				i := gtk_menu_item_new_with_label (
					"package variant: " & to_string (key (c))
					& " model " & package_model);
				
				i.on_activate (variant_selected'access);
				m.append (i);
				i.show;

				-- https://www.cc.gatech.edu/data_files/public/doc/gtk/tutorial/gtk_tut-14.html
			end query_variant;
			
		begin
			m := gtk_menu_new;
			variants.iterate (query_variant'access);

			m.show;
			m.popup;

		end show_variants_menu;
		
	begin
		device_model := to_file_name (file_selection_dialog (title => "Select a device model"));

		if type_device_model_file.length (device_model) > 0 then
			set_status ("selected device model: " & to_string (device_model));

			-- Read the device file and store it in the rig wide device 
			-- library et_devices.devices.
			-- If the device is already in the library, nothing happpens.
			read_device (
				file_name		=> device_model, -- ../lbr/logic_ttl/7400.dev
				log_threshold	=> log_threshold + 1);

			-- locate the device in the library
			device_cursor_lib := find (et_devices.devices, device_model);

			unit_add.device := device_cursor_lib;
			
			-- get the available package variants:
			variants := available_variants (device_cursor_lib);

			case element (device_cursor_lib).appearance is
				when PCB =>
					if length (variants) > 1 then
						show_variants_menu;
					else
						unit_add.variant := key (variants.first);
					end if;
					
				when VIRTUAL => null;
			end case;

		end if;
	end add_device;

	
	
-- PLACEHOLDERS

	procedure clarify_placeholder is
		use et_schematic.type_devices;
		use et_schematic.type_units;
		u : type_units.cursor;
		d : et_schematic.type_devices.cursor;
	begin
		-- On every call of this procedure we must advance from one
		-- placeholder to the next in a circular manner. So if the end 
		-- of the list is reached, then the cursor selected_name_placeholder
		-- moves back to the start of the placeholder list.
		if next (selected_placeholder) /= pac_proposed_placeholders.no_element then
			next (selected_placeholder);
		else
			selected_placeholder := proposed_placeholders.first;
		end if;

		-- show the selected placeholder in the status bar
		u := element (selected_placeholder).unit;
		d := element (selected_placeholder).device;
		
		set_status ("selected placeholder of " 
			& to_string (key (d)) 
			& "."
			& to_string (key (u)) 
			& ". " & status_next_object_clarification);
		
	end clarify_placeholder;
	
	procedure clear_proposed_placeholders is begin
		clear (proposed_placeholders);
		selected_placeholder := pac_proposed_placeholders.no_element;
	end clear_proposed_placeholders;
	
	procedure reset_placeholder is begin
		placeholder := (others => <>);
		clear_proposed_placeholders;
	end reset_placeholder;
	
	procedure finalize_move_placeholder (
		destination		: in type_point;
		category		: in type_placeholder_meaning;
		log_threshold	: in type_log_level)
	is
		su : type_selected_unit;

		use et_schematic.type_devices;
		use et_schematic.type_units;
	begin
		log (text => "finalizing move placeholder ...", level => log_threshold);
		log_indentation_up;

		if selected_placeholder /= pac_proposed_placeholders.no_element then
			su := element (selected_placeholder);

			move_unit_placeholder (
				module_name		=> et_project.modules.pac_generic_modules.key (current_active_module),
				device_name		=> key (su.device),
				unit_name		=> key (su.unit),
				coordinates		=> ABSOLUTE,
				point			=> destination,
				meaning			=> category,
				log_threshold	=> log_threshold);

			-- CS write a reduced procedure of move_unit_placeholder that takes a 
			-- module cursor, device cursor and unit cursor instead.

		else
			log (text => "nothing to do", level => log_threshold);
		end if;
			
		log_indentation_down;

		set_status (status_move_placeholder);
		
		reset_placeholder;
	end finalize_move_placeholder;
	
	function collect_placeholders (
		module			: in pac_generic_modules.cursor;
		place			: in et_coordinates.type_position; -- sheet/x/y
		catch_zone		: in type_catch_zone; -- the circular area around the place
		category		: in type_placeholder_meaning; -- name, value, purpose
		log_threshold	: in type_log_level)
		return pac_proposed_placeholders.list
	is
		result : pac_proposed_placeholders.list;

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in type_module) 
		is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor := module.devices.first;

			procedure query_units (
				device_name	: in type_name;
				device		: in et_schematic.type_device)
			is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor := device.units.first;

				placeholder_position : type_point;

				procedure test_placeholder_position is 
					pos_abs : et_coordinates.type_position;
				begin
					-- The current placeholder_position is relative to the unit position.
					-- It must be moved by the unit position in order to get the absolute position:
					move_by (placeholder_position, element (unit_cursor).position);

					-- Add the sheet information to the position:
					pos_abs := to_position (placeholder_position, sheet (place));
					
					--log (text => to_string (pos_abs), level => log_threshold + 1);

					-- Test whether the placeholder is inside the catch zone around the given place:
					if in_catch_zone (place, catch_zone, pos_abs) then
						log_indentation_up;

						log (text => "in catch zone", level => log_threshold + 1);
						result.append ((device_cursor, unit_cursor));
						
						log_indentation_down;
					end if;
				end test_placeholder_position;
		
			begin -- query_units
				while unit_cursor /= et_schematic.type_units.no_element loop
					
					-- We are interested in units on the given sheet only:
					if sheet (element (unit_cursor).position) = sheet (place) then

						log (text => "probing unit " & to_string (unit_cursor),
							level => log_threshold + 1);

						case category is
							when NAME =>
								-- Get the position of the name placeholder relative to the unit origin:
								placeholder_position := element (unit_cursor).name.position;
								test_placeholder_position;

							when VALUE =>
								-- Get the position of the value placeholder relative to the unit origin:
								placeholder_position := element (unit_cursor).value.position;
								test_placeholder_position;

							when PURPOSE =>
								-- Get the position of the purpose placeholder relative to the unit origin:
								placeholder_position := element (unit_cursor).purpose.position;
								test_placeholder_position;
								
						end case;
					end if;

					next (unit_cursor);
				end loop;
				
			end query_units;
			
		begin -- query_devices
			while device_cursor /= et_schematic.type_devices.no_element loop

				-- Only real devices have placeholders. Virtual devices are skipped here:
				if element (device_cursor).appearance = PCB then
				
					log (text => "probing device " & to_string (key (device_cursor)),
						level => log_threshold + 1);
					log_indentation_up;
						
					query_element (
						position	=> device_cursor,
						process		=> query_units'access);

				end if;
				
				next (device_cursor);

				log_indentation_down;
			end loop;
		end query_devices;

	begin -- collect_placeholders
		log (text => "looking up placeholders of category " 
			& enclose_in_quotes (to_string (category))
			& " at " & to_string (place) 
			& " catch zone" & to_string (catch_zone),
			level => log_threshold);

		log_indentation_up;
		
		query_element (
			position	=> module,
			process		=> query_devices'access);

		log_indentation_down;
		
		return result;
		
	end collect_placeholders;
	
	procedure find_placeholders (
		point		: in type_point;
		category	: in type_placeholder_meaning)
	is begin
		log (text => "locating placeholders of category " 
			 & enclose_in_quotes (to_string (category)) & " ...",
			 level => log_threshold);
		log_indentation_up;
		
		-- Collect all placeholders in the vicinity of the given point:
		proposed_placeholders := collect_placeholders (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			category		=> category,
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of placeholders found here:
		case length (proposed_placeholders) is
			when 0 =>
				reset_request_clarification;
				reset_placeholder;
				
			when 1 =>
				placeholder.being_moved := true;
				selected_placeholder := proposed_placeholders.first;

				set_status (status_move_placeholder);

				reset_request_clarification;
				
			when others =>
				set_request_clarification;

				-- preselect the first placeholder
				selected_placeholder := proposed_placeholders.first;
		end case;

		log_indentation_down;
	end find_placeholders;

	procedure rotate_placeholder (
		module_cursor	: in pac_generic_modules.cursor;
		unit			: in type_selected_unit;
		category		: in type_placeholder_meaning;
		log_threshold	: in type_log_level)
	is
		rotation : constant et_coordinates.type_rotation := 90.0;

		use et_schematic.type_devices;
		use et_schematic.type_units;

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			device_cursor : et_schematic.type_devices.cursor;

			procedure query_units (
				device_name	: in type_name;
				device		: in out et_schematic.type_device) 
			is 
				unit_cursor : et_schematic.type_units.cursor;

				-- Rotates the placeholder indicated by category
				-- by 90 degree. Since the rotation of placeholders
				-- is documentational, the rotation is always converted
				-- to HORIZONTAL or VERTICAL via function "snap":
				procedure rotate_placeholder (
					name	: in type_unit_name.bounded_string; -- A
					unit	: in out et_schematic.type_unit) 
				is
					r : type_rotation;
					use et_symbols.pac_text;
				begin
					case category is
						when et_symbols.NAME =>
							r := unit.name.rotation + rotation;
							unit.name.rotation := snap (r);
							
						when VALUE =>
							r := unit.value.rotation + rotation;
							unit.value.rotation := snap (r);
							
						when PURPOSE =>
							r := unit.purpose.rotation + rotation;
							unit.purpose.rotation := snap (r);

					end case;
				end rotate_placeholder;
				
			begin -- query_units
				-- Locate the given unit inside the device:
				unit_cursor := find (device.units, key (unit.unit));

				type_units.update_element (
					container	=> device.units,
					position	=> unit_cursor,
					process		=> rotate_placeholder'access);
				
			end query_units;
						
		begin -- query_devices
			-- Locate the given device inside the module:
			device_cursor := find (module.devices, key (unit.device));

			update_element (
				container	=> module.devices,
				position	=> device_cursor,
				process		=> query_units'access);

			
		end query_devices;
		
	begin -- rotate_placeholder
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) 
			 & " rotating " & to_string (key (unit.device)) & " unit " 
			 & to_string (key (unit.unit)) 
			 & " placeholder " & enclose_in_quotes (to_string (category))
			 & " by" & to_string (rotation) & " degree ...",
			 level => log_threshold);

		log_indentation_up;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);
		
		log_indentation_down;				
	end rotate_placeholder;
	
	procedure rotate_selected_placeholder (
		category	: in type_placeholder_meaning)
	is begin
		log (text => "rotating placeholder after clarification ...", level => log_threshold);
		log_indentation_up;

		rotate_placeholder (current_active_module, element (selected_placeholder), category, log_threshold + 1);
		
		log_indentation_down;
	end rotate_selected_placeholder;
	
	procedure rotate_placeholder (
		point 		: in type_point;
		category	: in type_placeholder_meaning)
	is begin
		log (text => "rotating placeholder ...",
			 level => log_threshold);

		log_indentation_up;
		
		-- Collect all placeholders in the vicinity of the given point:
		proposed_placeholders := collect_placeholders (
			module			=> current_active_module,
			place			=> to_position (point, current_active_sheet),
			catch_zone		=> catch_zone_default, -- CS should depend on current scale
			category		=> category,
			log_threshold	=> log_threshold + 1);

		-- evaluate the number of placeholders found here:
		case length (proposed_placeholders) is
			when 0 =>
				reset_request_clarification;
				reset_placeholder;
				
			when 1 =>
				selected_placeholder := proposed_placeholders.first;

				rotate_placeholder (current_active_module, element (selected_placeholder), category, log_threshold + 1);
				
				set_status (status_rotate_placeholder);

				reset_request_clarification;
				
			when others =>
				set_request_clarification;

				-- preselect the first placeholder
				selected_placeholder := proposed_placeholders.first;
		end case;
		
		log_indentation_down;
	end rotate_placeholder;
	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
