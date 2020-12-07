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
with ada.exceptions;				use ada.exceptions;

with glib;

with gdk.types;						use gdk.types;
with gdk.event;						use gdk.event;
with gdk.types.keysyms;				use gdk.types.keysyms;

with gtk.window;
with gtkada.file_selection;
with gtk.file_chooser_dialog;
with gtk.file_chooser;
with gtk.file_chooser_button;
with gtk.file_filter;

with gtk.main;
with gtk.widget;					use gtk.widget;
with gtk.window;
with gtk.box;
with gtk.label;
with gtk.gentry;
with gtk.menu;
with gtk.menu_item;
with gtk.menu_shell;

with et_general;					use et_general;
with et_geometry;					use et_geometry;
with et_devices;					use et_devices;
with et_device_rw;
with et_packages;
with et_schematic;					use et_schematic;
with et_material;
with et_meta;
with et_modes.schematic;			use et_modes.schematic;

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
				device_name	: in type_device_name;
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
				device_name	: in type_device_name;
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
				device_name	: in type_device_name;
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

	procedure reset_unit_move is begin
		unit_move := (others => <>);
		clear_proposed_units;
	end reset_unit_move;

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
		
		reset_unit_move;
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
		
		reset_unit_move;
	end finalize_drag;
	
	
	procedure find_units_for_move (point : in type_point) is 
		use et_modes.schematic;
	begin
		log (text => "locating units for move/drag ...", level => log_threshold);
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
				reset_unit_move;
				
			when 1 =>
				unit_move.being_moved := true;
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
	end find_units_for_move;

	procedure find_attached_segments is
		-- Device and unit name of the selected unit:
		use et_schematic.type_devices;
		use et_schematic.type_units;
		su : type_selected_unit := element (selected_unit);
		device_name : constant type_device_name := key (su.device);
		unit_name : constant pac_unit_name.bounded_string := key (su.unit);

		-- The ports with their positions of the selected unit:
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
						log (text => to_string (element (g)));
						
						if element (g).start_point = element (p).position then
							segments_being_dragged.append ((
-- 								net		=> n,
-- 								strand	=> s,
								segment	=> element (g),
								zone	=> START_POINT));

							--log (text => "dg S" & to_string (element (g)), console => true);
						end if;

						if element (g).end_point = element (p).position then
							segments_being_dragged.append ((
-- 								net		=> n,
-- 								strand	=> s,
								segment	=> element (g),
								zone	=> END_POINT));

							--log (text => "dg1 E" & to_string (element (g)), console => true);
						end if;
					end query_segment;
					
				begin -- query_strand
					if sheet (element (s).position) = current_active_sheet then
						iterate (element (s).segments, query_segment'access);
					end if;
				end query_strand;
				
			begin
				--log (text => "iterate strands", console => true);
				iterate (element (n).strands, query_strand'access);
			end query_net;
			
		begin
			--log (text => "iterate nets", console => true);
			iterate (element (current_active_module).nets, query_net'access);
		end query_port;
		
	begin -- find_attached_segments
		--log (text => "find attached segments", console => true);
		
		query_element (selected_unit, get_ports'access);
		-- now the ports of the selected unit are in "ports"

		--log (text => count_type'image (ports.length), console => true);
		
		et_symbols.type_ports.iterate (ports, query_port'access);
		
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
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device)
			is
				use et_schematic.type_units;
				unit_cursor : et_schematic.type_units.cursor;

				procedure rotate_unit (
					name	: in pac_unit_name.bounded_string; -- A
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

	function get_top_most_important_library return string is
		use et_meta;
		use et_meta.pac_preferred_libraries_schematic;
		all_lib_dirs : pac_preferred_libraries_schematic.list;
		top_lib_dir : pac_preferred_library_schematic.bounded_string;
	begin
		all_lib_dirs := get_preferred_libraries_schematic (current_active_module);
		top_lib_dir := element (all_lib_dirs.first);
		
		--return expand ("$HOME/git/BEL/ET_component_library/devices");
		return expand (to_string (top_lib_dir));
	end get_top_most_important_library;

	function device_selection_is_open return boolean is begin
		return device_selection.open;
	end device_selection_is_open;
	
	procedure close_device_selection is begin
		device_selection.window.destroy;
		device_selection.open := false;
	end close_device_selection;	

	function extract_variant_name (menu_item : in string) 
		return pac_package_variant_name.bounded_string 
	is
		-- Extract the variant name from field 3 of the menu item.
		-- Field separator is space:
		var_name : constant string := get_field_from_line (
			text_in		=> menu_item,
			position	=> 3);

		use et_devices;
	begin
		return to_variant_name (var_name);
	end extract_variant_name;
	
	-- In order to place a package variant and the associated model
	-- on a menu, use this function:
	function to_package_variant_item (variant : in pac_variants.cursor) 
		return string 
	is
		use pac_variants;
		variant_name : constant string := to_string (key (variant));

		use et_packages;
		package_model : constant string := to_string (element (variant).package_model);
	begin
		-- Build the menu item. NOTE: The actual variant name must be
		-- the 3rd string of the entry. Field separator is space.
		-- Procedures that evaluate the item expect it at this place:
		return "package variant: " & variant_name & " model: " & package_model;
	end to_package_variant_item;

	procedure reset_unit_add is begin
		unit_add := (others => <>);
	end reset_unit_add;
	
	procedure variant_selected (self : access gtk.menu_item.gtk_menu_item_record'class) is
	begin
		unit_add.variant := extract_variant_name (self.get_label);
		
		set_status ("Variant " 
			& enclose_in_quotes (to_string (unit_add.variant))
			& " selected. "
			& status_add);

		-- The device selection window is no longer needed:
		close_device_selection;
	end variant_selected;

	--procedure device_directory_selected (self : access gtk.file_chooser_button.gtk_file_chooser_button_record'class) is
	--begin
		--put_line (self.get_current_folder);
	--end device_directory_selected;
	
	procedure device_model_selected (self : access gtk.file_chooser_button.gtk_file_chooser_button_record'class) is

		use gtk.menu;
		use gtk.menu_item;
		use et_device_rw;
		use et_packages;
		
		device_model : pac_device_model_file.bounded_string;

		use pac_devices_lib;
		device_cursor_lib : pac_devices_lib.cursor; -- points to the device in the library

		unit_name : et_devices.pac_unit_name.bounded_string;
		
		use pac_variants;
		variants : pac_variants.map;
		
		procedure show_variants_menu is
			m : gtk_menu;
			i : gtk_menu_item;
			
			procedure query_variant (c : in pac_variants.cursor) is begin
				-- Build the menu item:
				i := gtk_menu_item_new_with_label (to_package_variant_item (c));
				i.on_activate (variant_selected'access);
				m.append (i);
				i.show;
			end query_variant;
			
		begin
			m := gtk_menu_new;
			variants.iterate (query_variant'access);

			m.show;
			m.popup;

		end show_variants_menu;

	begin -- device_model_selected
		put_line (self.get_filename);
		
		device_model := to_file_name (self.get_filename);

		--set_status ("selected device model: " & to_string (device_model));

		-- Read the device file and store it in the rig wide device 
		-- library et_devices.devices.
		-- If the device is already in the library, nothing happpens.
		read_device (
			file_name		=> device_model, -- ../lbr/logic_ttl/7400.dev
			log_threshold	=> log_threshold + 1);

		-- locate the device in the library
		device_cursor_lib := find (et_devices.devices, device_model);

		-- assign the cursor to the device model:
		unit_add.device := device_cursor_lib;

		-- Assign the name of the first unit.
		-- NOTE: When adding a device, the first unit within the device
		-- will be placed first. Further units are to be placed via
		-- invoke operations:
		unit_add.name := first_unit (device_cursor_lib);

		-- For a nice preview we also need the total of units provided
		-- the the device:
		unit_add.total := units_total (unit_add.device);
		
		-- assign the prospective device name:
		unit_add.device_pre := next_device_name (current_active_module, element (device_cursor_lib).prefix);
		
		-- get the available package variants:
		variants := available_variants (device_cursor_lib);

		case element (device_cursor_lib).appearance is
			when PCB =>
				if length (variants) > 1 then
					show_variants_menu;
				else
					unit_add.variant := key (variants.first);

					-- The device selection window is no longer needed:
					close_device_selection;
					
					set_status (status_add);
				end if;
				
			when VIRTUAL => null;
		end case;
			
		-- CS exception handler in case read_device fails ?
	end device_model_selected;

	
	function device_model_selection_key_event (
		self	: access gtk_widget_record'class;
		event	: in gdk_event_key) 
		return boolean 
	is
		key : gdk_key_type := event.keyval;

		-- This is required in order to propagate the key-pressed event further.
		result : boolean; -- to be returned. Indicates that the event has been handled.
	begin
		case key is
			when GDK_ESCAPE =>
				--put_line ("key A");

				-- Close the device selection if operator hits ESC:
				close_device_selection;
				result := true;

			when others =>
				--put_line ("key B");
				result := false;
		end case;
		
		return result;
	end device_model_selection_key_event;

	procedure device_model_selection_close (
		self	: access gtk_widget_record'class) 
	is begin
		device_selection.open := false;
	end device_model_selection_close;
	
	procedure add_device is
		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.file_chooser_dialog;
		use gtk.file_chooser;
		use gtk.file_chooser_button;
		use gtk.file_filter;

		box_main : gtk_hbox;
		box_directory, box_model : gtk_vbox;
		label_directory, label_model : gtk_label;
		
		use glib;
		spacing : constant natural := 10;
		
		button_directory, button_model : gtk_file_chooser_button;
		filter : gtk_file_filter;
		
	begin -- add_device
		--put_line ("device selection");

		-- BUILD THE DEVICE SELECTION WINDOW:

		-- Build it if it is not already up:
		if not device_selection_is_open then

			-- Mark the window as "open" so that it can not be opened anew:
			device_selection.open := true;
			
			gtk_new (device_selection.window);
			device_selection.window.set_title ("Select a device model");
			device_selection.window.on_destroy (device_model_selection_close'access);
			device_selection.window.on_key_press_event (device_model_selection_key_event'access);

			
			gtk_new_hbox (box_main, homogeneous => false, spacing => gint (spacing));
			add (device_selection.window, box_main);

			-- directory selection on the LEFT:
			gtk_new_vbox (box_directory, homogeneous => false);
			pack_start (box_main, box_directory, padding => guint (spacing));

			gtk_new (label_directory, "directory");
			
			gtk_new (
				button		=> button_directory,
				title		=> "Select a device model",
				action		=> ACTION_SELECT_FOLDER);

			-- CS: Currently the button_directory shows only the most important
			-- library path. It would be convenient if the operator would be shown
			-- all preferred library paths sorted by their rank.
			if button_directory.set_current_folder_uri (get_top_most_important_library) then
				null; -- Testing the existence of the folder is not required.
			end if;
			
			--button_directory.on_file_set (device_directory_selected'access);
			pack_start (box_directory, label_directory, padding => guint (spacing));
			pack_start (box_directory, button_directory, padding => guint (spacing));

			
			-- device model selection on the RIGHT:		
			gtk_new_vbox (box_model, homogeneous => false);
			pack_start (box_main, box_model, padding => guint (spacing));

			gtk_new (label_model, "model");
			
			gtk_new (filter);
			add_pattern (filter, make_filter_pattern (device_model_file_extension));
			set_name (filter, "Device Models");
			
			gtk_new (
				button		=> button_model,
				title		=> "Select a device model",
				action		=> ACTION_OPEN);

			button_model.add_filter (filter);

			if button_model.set_current_folder (button_directory.get_current_folder_uri) then
				null; -- Testing the existence of the device model is not required.
			end if;
			
			button_model.on_file_set (device_model_selected'access);
			pack_start (box_model, label_model, padding => guint (spacing));
			pack_start (box_model, button_model, padding => guint (spacing));
			
			device_selection.window.show_all;
		end if;
	end add_device;



	
	procedure finalize_add_device (
		position	: in type_point)
	is 
		use et_devices;
		use pac_devices_lib;
	begin

		add_device (
			module_name		=> key (current_active_module),
			device_model	=> pac_devices_lib.key (unit_add.device),
			variant			=> unit_add.variant,
			destination		=> to_position (position, current_active_sheet),
			log_threshold	=> log_threshold + 1);
		
		reset_unit_add;
		reset_activate_counter;

		set_status (status_add);
		--status_enter_verb;
	end finalize_add_device;

	procedure finalize_invoke (
		position		: in type_point;
		log_threshold	: in type_log_level)
	is 
		use pac_unit_name;
	begin
		log (text => "finalizing invoke ...", level => log_threshold);
		log_indentation_up;
		
		if length (unit_add.name) > 0 then
			
			invoke_unit (
				module_name		=> key (current_active_module),
				device_name		=> unit_add.device_pre,
				unit_name		=> unit_add.name,
				destination		=> to_position (position, current_active_sheet),
				log_threshold	=> log_threshold + 1);
		else
			log (text => "nothing to do", level => log_threshold + 1);
		end if;

		reset_unit_add;
		reset_request_clarification;
		set_status (status_invoke);

		log_indentation_down;
		
	end finalize_invoke;
		

	-- Extracts from the selected menu item the unit name.
	procedure unit_selected (self : access gtk.menu_item.gtk_menu_item_record'class) is
		
		-- Extract the unit name from field 2 of the menu item:
		unit_name : constant string := get_field_from_line (
			text_in		=> self.get_label,
			position	=> 2);
	begin
		--put_line ("selected");
		
		-- assign the unit to be drawn:
		unit_add.name := to_unit_name (unit_name);

		-- Signal procedure draw_units to draw this unit as a preview:
		unit_add.via_invoke := true;

		-- The list of proposed units and the cursor "selected_unit" are
		-- no longer required. This operation also prevents the formerly
		-- selected unit to be drawn highlighted:
		clear_proposed_units;
		
		set_status ("Device " & to_string (unit_add.device_pre) 
			& " unit " & unit_name & " selected.");

	end unit_selected;

	procedure unit_selection_cancelled (self : access gtk.menu_shell.gtk_menu_shell_record'class) is
	begin
		set_status ("Unit selection cancelled");
		reset_unit_add;

		--put_line ("deselected");
	end unit_selection_cancelled;

	-- CS
	procedure set_position ( -- of a menu
		menu : not null access gtk.menu.gtk_menu_record'class;
		x : out glib.gint;
		y : out glib.gint;
		push_in : out boolean)
	is
		use glib;
		--use et_coordinates.pac_geometry_sch;

		--cp : type_point := cursor_main.position;
		--mp : type_point := canvas.drawing_to_model (cursor_main.position);
		--vp : type_view_point;

		use gtk.widget;
		
		a : gtk_allocation;
	begin
		canvas.get_allocation (a);

		-- gdk.window.get_position
		
		--vp := canvas.model_to_view (mp);

		--put_line (to_string (vp));
		
		--x := gint (type_view_coordinate (cp.x));
		--y := gint (vp.y);

		x := a.x;
		y := a.y;
		
		push_in := true;
	end set_position;
	
	procedure show_units is
		use et_schematic.type_devices;
		
		su : constant type_selected_unit := element (selected_unit);
		
		device_model : pac_device_model_file.bounded_string;
		device_cursor_lib : pac_devices_lib.cursor;
		
		unit_names : pac_unit_names.list;
		
		procedure show_menu is

			--use glib;
			use gtk.menu;
			use gtk.menu_item;

			m : gtk_menu; -- the menu
			i : gtk_menu_item; -- an item on the menu
									   
			-- If no units are available, then no menu is to be shown.
			-- So we must count units with this stuff:
			subtype type_units_available is natural range 0 .. type_unit_count'last;
			units_available : type_units_available := 0;
			
			use pac_unit_names;
			
			procedure query_name (c : in pac_unit_names.cursor) is 

				in_use : boolean := false;

				-- Sets the in_use flag if given unit is already in use:
				procedure query_in_use (
					device_name	: in type_device_name;
					device		: in et_schematic.type_device) 
				is
					use et_schematic.type_units;
				begin
					if contains (device.units, element (c)) then
						in_use := true;
					end if;
				end query_in_use;

			begin -- query_name
				-- Test whether the unit is already in use.
				query_element (
					position	=> su.device,
					process		=> query_in_use'access);

				-- If the unit is available then put its name on the menu:
				if not in_use then -- unit is available
					
					units_available := units_available + 1;
					
					-- Build the menu item. NOTE: The actual unit name must be
					-- the 2nd string of the entry. Procedure unit_selected expects
					-- it at this place:
					i := gtk_menu_item_new_with_label (
						"unit " & to_string (element (c)));

					-- Connect the item with the "activate" signal which
					-- in turn calls procedure unit_selected:
					i.on_activate (unit_selected'access);

					m.append (i);
					i.show;
				end if;

			end query_name;
			
		begin -- show_menu

			-- create a menu
			m := gtk_menu_new;

			-- In case the operator closes the menu (via ESC for example)
			-- then reset unit_add.
			m.on_cancel (unit_selection_cancelled'access);
			
			unit_names.iterate (query_name'access);

			-- If no units are available (because all are in use)
			-- then we do not show a menu.
			if units_available > 0 then
				m.show;
				m.popup (
					-- CS func => set_position'access,
							
					-- button 0 means: this is not triggered by a key press
					-- or a button click:
					button => 0,
							
					-- get_current_event_time causes the menu to remain
					-- until a 2nd click.
					activate_time => gtk.main.get_current_event_time);
			else
				set_status ("No more units of device " 
					& to_string (unit_add.device_pre)
					& " available !");

				reset_unit_add;
				--set_status (status_invoke);
			end if;
		end show_menu;
		
	begin -- show_units
		--put_line ("selected " & to_string (key (su.device)));

		device_model := element (su.device).model;

		--put_line ("model " & to_string (device_model));
		
		device_cursor_lib := locate_device (device_model);

		-- assign the cursor to the device model:
		unit_add.device := device_cursor_lib;

		-- For a nice preview we also need the total of units provided
		-- the the device:
		unit_add.total := units_total (unit_add.device);
			
		-- assign the prospective device name:
		unit_add.device_pre := key (su.device);

		-- collect the names of all units of the selected device:
		unit_names := all_units (device_cursor_lib);

		-- Show the units of the device in a menu. After the operator
		-- has selected a unit, procedure unit_selected finally
		-- assigns the unit name to unit_add.
		show_menu;
		
	end show_units;

	
	procedure invoke_unit (point : in type_point) is 
		use et_schematic_ops.units;
	begin
		log (text => "invoking unit ...", level => log_threshold);
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
				selected_unit := proposed_units.first;
			
				show_units;

			when others =>
				set_request_clarification;

				-- preselect the first unit
				selected_unit := proposed_units.first;
		end case;
		
		log_indentation_down;
	end invoke_unit;
	
	
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
		placeholder_move := (others => <>);
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
				device_name	: in type_device_name;
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
				placeholder_move.being_moved := true;
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
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) 
			is 
				unit_cursor : et_schematic.type_units.cursor;

				-- Rotates the placeholder indicated by category
				-- by 90 degree. Since the rotation of placeholders
				-- is documentational, the rotation is always converted
				-- to HORIZONTAL or VERTICAL via function "snap":
				procedure rotate_placeholder (
					name	: in pac_unit_name.bounded_string; -- A
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


-- SET PROPERTIES SUCH AS NAME, VALUE, PURPOSE, PARCODE

	-- Called when the operator presses ENTER after typing a property in
	-- the properties window.
	-- The properties window will remain open until the operator enters 
	-- a correct property. The status bar of the window shows the error message:	
	procedure property_entered (self : access gtk.gentry.gtk_entry_record'class) is 
		su : type_selected_unit := element (selected_unit);

		use et_schematic.type_devices;

		value	: type_value.bounded_string;
		purpose	: pac_device_purpose.bounded_string;
		variant	: pac_package_variant_name.bounded_string;

		use et_material;
		partcode : type_partcode.bounded_string;

		procedure clean_up is begin
			properties_confirmed := true;
			window_properties.window.destroy;
			reset_request_clarification;
			--status_clear;
			clear_proposed_units;
			redraw;
		end clean_up;
		
	begin -- property_entered
		case noun is
			when NOUN_DEVICE =>
				rename_device (
					module_name 		=> key (current_active_module),
					device_name_before	=> key (su.device), -- IC2
					device_name_after	=> to_device_name (self.get_text), -- IC3
					log_threshold		=> log_threshold + 1);

				-- CS use rename_device that takes a module cursor and a device cursor
				-- for device_name_before
				
			when NOUN_PARTCODE =>
				partcode := to_partcode (self.get_text);

				set_partcode (
					module_name		=> key (current_active_module),
					device_name		=> key (su.device),
					partcode		=> partcode,
					log_threshold	=> log_threshold + 1);

				-- CS use set_partcode that takes a module cursor and a device cursor
				
			when NOUN_PURPOSE =>
				purpose := to_purpose (self.get_text);
				
				set_purpose (
					module_name		=> key (current_active_module),
					device_name		=> key (su.device),
					purpose			=> purpose,
					log_threshold	=> log_threshold + 1);

				-- CS use set_purpose that takes a module cursor and a device cursor
				
			when NOUN_VALUE =>
				value := to_value_with_check (self.get_text);

				set_value (
					module_name		=> key (current_active_module),
					device_name		=> key (su.device),
					value			=> value,
					log_threshold	=> log_threshold + 1);

				-- CS use set_value that takes a module cursor and a device cursor

			when NOUN_VARIANT =>
				check_variant_name_length (self.get_text);
				variant := to_variant_name (self.get_text);
				check_variant_name_characters (variant);

				set_variant (
					module			=> current_active_module,
					device			=> su.device,
					variant			=> variant);
				
			when others => raise constraint_error;
		end case;

		-- If everything was fine, close the window and clean up.
		-- If one of the operations above has raised an exception then
		-- nothing will be cleaned up and the window will remain until the
		-- operator enters a correct property.
		clean_up;
		
		-- Whatever goes wrong, output the message in the status bar
		-- of the properties window:
		exception when event: others =>
			set_status_properties (exception_message (event));
			
	end property_entered;

	
	procedure window_set_property is
		use gtk.window;
		use gtk.box;
		use gtk.label;
		use gtk.gentry;
		
		box : gtk_vbox;
		label : gtk_label;
		gentry : gtk_gentry;
		
		su : type_selected_unit := element (selected_unit);

		use et_schematic.type_devices;
		device_name : constant string := to_string (key (su.device)); -- IC2
	begin		
		-- Properties of real devices can be changed.
		-- Virtual devices (like GND symbols) can not be changed.
		if is_real (su.device) then
			build_window_properties;

			window_properties.window.set_default_size (200, 100);
			window_properties.window.set_resizable (false);
			
			gtk_new_vbox (box);
			add (window_properties.window, box);

			-- Prepare displaying the old state of the property:
			gtk_new (entry_property_old);
			
			case noun is
				when NOUN_DEVICE =>
					gtk_new (label, "Name of " & device_name);
					set_property_before (device_name); -- IC2

				when NOUN_PARTCODE =>
					gtk_new (label, "Partcode of " & device_name);
					set_property_before (et_material.to_string (get_partcode (su.device)));

				when NOUN_PURPOSE =>
					gtk_new (label, "Purpose of " & device_name);
					set_property_before (et_devices.to_string (get_purpose (su.device)));
					
				when NOUN_VALUE =>
					gtk_new (label, "Value of " & device_name);
					set_property_before (et_devices.to_string (get_value (su.device)));

				when NOUN_VARIANT =>
					gtk_new (label, "Package variant of " & device_name);
					set_property_before (et_devices.to_string (get_variant (su.device)));
					
				when others => raise constraint_error;
			end case;				

			pack_start (box, label);

			-- show the old property:
			gtk_new (label_property_old, "old:");
			pack_start (box, label_property_old);
			pack_start (box, entry_property_old);

			-- show the new property (will be entered by the operator later):
			gtk_new (label_property_new, "new:");
			pack_start (box, label_property_new);
			
			gtk_new (gentry); -- CS if NOUN_VARIANT propose available variants here
			pack_start (box, gentry);
			gentry.on_activate (property_entered'access);
			gentry.grab_focus;

			gtk_new (label_properties_status);
			pack_start (box, label_properties_status);
			
			window_properties.window.show_all;
		else
			set_status ("ERROR: Device " & device_name & " is virtual !");
		end if;
	end window_set_property;

	
	procedure set_property (point : in type_point) is begin
		-- If the properties window is already open, then it
		-- is moved to the foreground.
		if not window_properties_is_open then
			log (text => "setting property ...", level => log_threshold);
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
					selected_unit := proposed_units.first;

					window_set_property;

				when others =>
					set_request_clarification;

					-- preselect the first unit
					selected_unit := proposed_units.first;
			end case;

			log_indentation_down;
		else
			log (text => "Window to set properties already open !", level => log_threshold);

			-- Move the properties window to the foreground so that the operator
			-- is notified about the already open properties window:
			window_properties.window.present;
		end if;
	end set_property;

	procedure set_property_selected_unit is
		use et_schematic_ops.units;
	begin
		log (text => "setting property of unit/device after clarification ...", level => log_threshold);
		log_indentation_up;

		window_set_property;
		
		log_indentation_down;
	end set_property_selected_unit;


	procedure show_properties_of_selected_device
	is
		use et_schematic.type_devices;
		use et_schematic.type_units;
		
		su		: constant type_selected_unit := element (selected_unit);

		--device	: constant et_devices.type_device_name := key (su.device);
		
		-- NOTE: In case the unit name is required for some reason,
		-- mind the cursor su.unit can be no_element if all units are selected.
		--unit	: constant pac_unit_name.bounded_string := key (su.unit);

		model	: constant pac_device_model_file.bounded_string := element (su.device).model;

		function further_properties return string is 
			use et_material;
			var	: constant string := ", variant ";
			pc	: constant string := ", partcode ";
		begin
			case element (su.device).appearance is
				when PCB =>
					return var & to_string (element (su.device).variant)
						& pc & to_string (element (su.device).partcode);

				when others => return "";
			end case;
		end further_properties;
	
	begin
		reset_request_clarification;
		
		set_status ("Properties:"
			& " Model " & to_string (model)
			& further_properties); -- variant, partcode, ...	
		
	end show_properties_of_selected_device;
	
	procedure find_units_for_show (point : in type_point) is 
		use et_modes.schematic;
	begin
		log (text => "locating units for show ...", level => log_threshold);
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
				selected_unit := proposed_units.first;
				show_properties_of_selected_device;
				
			when others =>
				set_request_clarification;

				-- preselect the first unit
				selected_unit := proposed_units.first;
		end case;
		
		log_indentation_down;
	end find_units_for_show;

	
end et_canvas_schematic_units;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
