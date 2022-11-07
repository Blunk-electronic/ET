------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       BOARD OPERATIONS / DEVICES                         --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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

with ada.exceptions;

with et_netlists;
with et_schematic.device_query_ops;	use et_schematic.device_query_ops;
with et_schematic_ops;				use et_schematic_ops;

with et_submodules;
with et_pcb_rw.device_packages;

with et_contour_to_polygon;			use et_contour_to_polygon;



package body et_board_ops.devices is

	use pac_generic_modules;

	use pac_devices_sch;
	use pac_devices_non_electric;
	use pac_nets;

	
	procedure terminal_not_found (terminal_name : in pac_terminal_name.bounded_string) is begin
		log (ERROR,	"terminal " & enclose_in_quotes (to_string (terminal_name)) & " not found !",
			 console => true);
		raise constraint_error;
	end terminal_not_found;



	function get_placeholders (
		package_cursor : in et_packages.pac_packages_lib.cursor)
		return et_packages.type_text_placeholders 
	is
		use et_packages;
		use pac_packages_lib;
	begin
		return p : type_text_placeholders do
		
			-- fetch the placeholders of silk screen top and bottom
			p.silk_screen.top := element (package_cursor).silk_screen.top.placeholders;
			p.silk_screen.bottom := element (package_cursor).silk_screen.bottom.placeholders;

			-- fetch the placeholders of assembly documentation top and bottom
			p.assy_doc.top := element (package_cursor).assembly_documentation.top.placeholders;
		p.assy_doc.bottom := element (package_cursor).assembly_documentation.bottom.placeholders;
		
		end returN;
	end get_placeholders;

	
	procedure add_device ( -- non-electric !
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		package_model	: in et_packages.pac_package_model_file_name.bounded_string; -- ../lbr/packages/fiducial.pac
		position		: in type_package_position; -- x,y,rotation,face
		prefix			: in pac_device_prefix.bounded_string; -- FD
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		package_cursor_lib : et_packages.pac_packages_lib.cursor;
		
		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			device_cursor : pac_devices_non_electric.cursor;
			inserted : boolean;

			-- build the next available device name:
			next_name : type_device_name := next_device_name (module_cursor, prefix, NON_ELECTRICAL);
		begin -- add
			log (text => "adding device " & to_string (next_name), level => log_threshold + 1);
			log_indentation_up;

			-- add the device to the collection of non-electic devices:
			pac_devices_non_electric.insert (
				container	=> module.devices_non_electric,
				inserted	=> inserted,
				position	=> device_cursor,
				key			=> next_name,
				new_item	=> (
					position			=> position,
					package_model		=> package_model,
					text_placeholders	=> get_placeholders (package_cursor_lib),
					flipped				=> NO -- CS
					)
				);

			-- check inserted flag
			if not inserted then
				raise constraint_error;
			end if;

			log_indentation_down;
		end add;
		
	begin -- add_device
		log (text => "module " & to_string (module_name) &
			" adding non-electric device " & to_string (package_model) &
			" at" &
			to_string (position),
			level => log_threshold);

		log_indentation_up;
		
		-- locate module
		module_cursor := locate_module (module_name);

		-- Read the package model (like ../libraries/fiducials/crosshair.pac)
		-- and store it in the rig wide package library et_packages.packages.
		-- If it s already in the library, nothing happens:
		et_pcb_rw.device_packages.read_package (
			file_name		=> package_model,
-- CS						check_layers	=> YES,
			log_threshold	=> log_threshold + 1);

		-- locate the package in the library
		package_cursor_lib := locate_package_model (package_model);

		-- add the device to the module
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);
		
		log_indentation_down;
	end add_device;

	
	procedure move_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			device_electric		: pac_devices_sch.cursor;
			device_non_electric	: pac_devices_non_electric.cursor;			

			
			procedure set_position ( -- of an electric device
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set (point => device.position.place, position => point); 
						-- preserve angle and face

					when RELATIVE =>
						move_by (point => device.position.place, offset => to_distance_relative (point));
						-- preserve angle and face
						
				end case;
			end set_position;

			
			procedure set_position ( -- of a non-electric device
				device_name	: in type_device_name;
				device		: in out type_device_non_electric) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set (point => device.position.place, position => point); 
						-- preserve angle and face

					when RELATIVE =>
						move_by (point => device.position.place, offset => to_distance_relative (point)); 
						-- preserve angle and face
						
				end case;
			end set_position;

			
		begin -- query_devices

			-- Search the device first among the electric devices.
			-- Most likely it will be among them. If not,
			-- search in non-electric devices.
			if contains (module.devices, device_name) then

				device_electric := find (module.devices, device_name);

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_electric,
					process		=> set_position'access);

			else
				-- search among non-electric devices:
				if contains (module.devices_non_electric, device_name) then

					device_non_electric := find (module.devices_non_electric, device_name);

					-- set new position
					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electric,
						process		=> set_position'access);

				else
					device_not_found (device_name);
				end if;
			end if;
		end query_devices;

		
	begin -- move_device
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving device " & to_string (device_name) &
					" to" & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving device " & to_string (device_name) &
					" by" & to_string (point), level => log_threshold);
		end case;

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end move_device;

	
	procedure rotate_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_pcb_coordinates.type_rotation; -- 90
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			device_electric		: pac_devices_sch.cursor;
			device_non_electric	: pac_devices_non_electric.cursor;			

			procedure set_rotation ( -- of an electric device
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set (device.position, rotation); -- preserve x/y and face

					when RELATIVE =>
						rotate_about_itself (position => device.position, offset => rotation); -- preserve x/y and face
				end case;
			end;

			
			procedure set_rotation ( -- of a non-electric device
				device_name	: in type_device_name;
				device		: in out type_device_non_electric) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set (device.position, rotation); -- preserve x/y and face

					when RELATIVE =>
						rotate_about_itself (position => device.position, offset => rotation); -- preserve x/y and face
				end case;
			end;
			
		begin -- query_devices

			-- Search the device first among the electric devices.
			-- Most likely it will be among them. If not,
			-- search in non-electric devices.
			if contains (module.devices, device_name) then

				device_electric := find (module.devices, device_name);

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_electric,
					process		=> set_rotation'access);

			else
				-- search among non-electric devices:
				if contains (module.devices_non_electric, device_name) then

					device_non_electric := find (module.devices_non_electric, device_name);

					-- set new position
					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electric,
						process		=> set_rotation'access);

				else
					device_not_found (device_name);
				end if;
			end if;

		end query_devices;

		
	begin -- rotate_device
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" rotating device " & to_string (device_name) &
					" to" & to_string (rotation), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" rotating device " & to_string (device_name) &
					" by" & to_string (rotation), level => log_threshold);
		end case;

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end rotate_device;

	
	procedure delete_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- FD1
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			-- Search the device among the non-electric devices.
			if contains (module.devices_non_electric, device_name) then

				delete (module.devices_non_electric, device_name);
			else
				device_not_found (device_name);
			end if;
		end query_devices;

		
	begin -- delete_device
		log (text => "module " & to_string (module_name) &
			 " deleting device (non-electric) " & to_string (device_name),
			 level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end delete_device;

	
	procedure rename_device (
		module_name			: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name_before	: in type_device_name; -- FD1
		device_name_after	: in type_device_name; -- FD3
		log_threshold		: in type_log_level) 
	is		
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			device_before, device_after : pac_devices_non_electric.cursor;

			inserted : boolean;
		begin
			-- Search the old device among the non-electric devices.
			if contains (module.devices_non_electric, device_name_before) then

				-- locate the device by the old name
				device_before := find (module.devices_non_electric, device_name_before); -- FD1
				
				-- copy elements and properties of the old device to a new one:
				pac_devices_non_electric.insert (
					container	=> module.devices_non_electric,
					key			=> device_name_after, -- FD3
					new_item	=> element (device_before), -- all elements and properties of FD1
					inserted	=> inserted,
					position	=> device_after);

				if not inserted then
					device_already_exists (device_name_after);
				end if;

				-- check conformity of prefix
				if not et_conventions.prefix_valid (device_name_after) then
					null;
					--device_prefix_invalid (device_after);
				end if;

				-- delete the old device
				delete (module.devices_non_electric, device_before);
				
			else
				device_not_found (device_name_before);
			end if;
		end query_devices;

		
	begin -- rename_device
		log (text => "module " & to_string (module_name) &
			 " renaming device (non-electric) " & to_string (device_name_before) & " to " & 
			to_string (device_name_after),
			level => log_threshold);
		
		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rename_device;
	
	
	procedure flip_device (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		face			: in type_face; -- top/bottom
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			device_electric		: pac_devices_sch.cursor;
			device_non_electric	: pac_devices_non_electric.cursor;			

			scratch : et_packages.pac_text_placeholders.list;
			
			-- Mirrors the position of a placeholder along the y-axis:
			procedure mirror_placeholder (
				p : in out et_packages.type_text_placeholder) 
			is begin
				mirror (point => p.position.place, axis => Y);
			end mirror_placeholder;

			
			procedure mirror_placeholders (phs : in out et_packages.pac_text_placeholders.list) is 
				use et_packages.pac_text_placeholders;
				cursor : et_packages.pac_text_placeholders.cursor := phs.first;
			begin
				while cursor /= et_packages.pac_text_placeholders.no_element loop
						et_packages.pac_text_placeholders.update_element (
							container	=> phs,
							position	=> cursor,
							process		=> mirror_placeholder'access);
					next (cursor);
				end loop;
			end mirror_placeholders;

			
			procedure flip ( -- electric device
				device_name	: in type_device_name;
				device		: in out type_device_sch) 
			is				
				face_before : constant type_face := get_face (device.position);
			begin
				if face_before /= face then
					set_face (position => device.position, face => face); -- preserve x/y and rotation

					-- toggle the flipped flag
					if device.flipped = NO then
						device.flipped := YES;
					else
						device.flipped := NO;
					end if;

					-- SILKSCREEN
					-- swap placeholders top/bottom
					scratch := device.text_placeholders.silk_screen.bottom;
					device.text_placeholders.silk_screen.bottom := device.text_placeholders.silk_screen.top;
					device.text_placeholders.silk_screen.top := scratch;

					-- mirror
					mirror_placeholders (device.text_placeholders.silk_screen.top);
					mirror_placeholders (device.text_placeholders.silk_screen.bottom);
					
					-- ASSEMBLY DOCUMENTATION
					-- swap placeholders top/bottom
					scratch := device.text_placeholders.assy_doc.bottom;
					device.text_placeholders.assy_doc.bottom := device.text_placeholders.assy_doc.top;
					device.text_placeholders.assy_doc.top := scratch;

					-- mirror
					mirror_placeholders (device.text_placeholders.assy_doc.top);
					mirror_placeholders (device.text_placeholders.assy_doc.bottom);
					
				else
					log (WARNING, "package already on " & to_string (face) & " !");
				end if;
			end flip;

			
			procedure flip ( -- non-electric device
				device_name	: in type_device_name;
				device		: in out type_device_non_electric) 
			is				
				face_before : constant type_face := get_face (device.position);
			begin
				if face_before /= face then
					set_face (position => device.position, face => face); -- preserve x/y and rotation

					-- toggle the flipped flag
					if device.flipped = NO then
						device.flipped := YES;
					else
						device.flipped := NO;
					end if;

					-- SILKSCREEN
					-- swap placeholders top/bottom
					scratch := device.text_placeholders.silk_screen.bottom;
					device.text_placeholders.silk_screen.bottom := device.text_placeholders.silk_screen.top;
					device.text_placeholders.silk_screen.top := scratch;

					-- mirror
					mirror_placeholders (device.text_placeholders.silk_screen.top);
					mirror_placeholders (device.text_placeholders.silk_screen.bottom);
					
					-- ASSEMBLY DOCUMENTATION
					-- swap placeholders top/bottom
					scratch := device.text_placeholders.assy_doc.bottom;
					device.text_placeholders.assy_doc.bottom := device.text_placeholders.assy_doc.top;
					device.text_placeholders.assy_doc.top := scratch;

					-- mirror
					mirror_placeholders (device.text_placeholders.assy_doc.top);
					mirror_placeholders (device.text_placeholders.assy_doc.bottom);
					
				else
					log (WARNING, "package already on " & to_string (face) & " !");
				end if;
			end flip;

			
		begin -- query_devices

			-- Search the device first among the electric devices.
			-- Most likely it will be among them. If not,
			-- search in non-electric devices.
			if contains (module.devices, device_name) then

				device_electric := find (module.devices, device_name);

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_electric,
					process		=> flip'access);

			else
				-- search among non-electric devices:
				if contains (module.devices_non_electric, device_name) then

					device_non_electric := find (module.devices_non_electric, device_name);

					-- set new position
					update_element (
						container	=> module.devices_non_electric,
						position	=> device_non_electric,
						process		=> flip'access);

				else
					device_not_found (device_name);
				end if;

			end if;
		end query_devices;

		
	begin -- flip_device
		log (text => "module " & to_string (module_name) &
			" flipping device " & to_string (device_name) &
			" to" & to_string (face), level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_devices'access);


		update_ratsnest (module_cursor, log_threshold + 1);		
	end flip_device;

	
	-- Returns the position (x/y/rotation) of a submodule instance.
	-- Assumptions:
	--  - The module to be searched in must be in the rig already.
	--  - The submodule instance must exist in the module.
	function get_position (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in et_general.pac_module_instance_name.bounded_string) -- OSC1
		return type_position 
	is		
		position : type_position := origin_zero_rotation; -- to be returned

		module_cursor : pac_generic_modules.cursor; -- points to the module

		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_module) 
		is
			use et_submodules.pac_submodules;
			submod_cursor : et_submodules.pac_submodules.cursor;
		begin
			submod_cursor := find (module.submods, instance);
			position := element (submod_cursor).position_in_board;
		end;
		
	begin -- get_position
		-- locate the given module
		module_cursor := locate_module (module_name);

		pac_generic_modules.query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);

		return position;
	end get_position;

	
	function get_terminal_position (
		module_cursor	: in et_project.modules.pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor; -- IC45
		terminal_name	: in pac_terminal_name.bounded_string) -- H7, 14
		return type_terminal_position_fine
	is
		-- This is the position of the package as it is in the layout:
		package_position : et_pcb_coordinates.type_package_position; -- incl. angle and face

		use pac_geometry_brd;
		terminal_position : type_vector; -- x/y
		terminal_rotation : type_angle;
		terminal_position_face : type_face := TOP; -- top/bottom

		model : pac_package_model_file_name.bounded_string; -- libraries/packages/smd/SOT23.pac
		package_model_cursor : pac_packages_lib.cursor;

		use pac_terminals;
		-- This cursor points to the terminal in the package model:
		terminal_cursor : pac_terminals.cursor;
		
		terminal_technology : type_assembly_technology;
		
	begin
		-- Get the package model of the given device:
		model := get_package_model (device_cursor);

		-- Get the position of the package as it is in the layout:
		package_position := pac_devices_sch.element (device_cursor).position;
		
		-- Set the cursor to package model:
		package_model_cursor := locate_package_model (model);

		-- Locate the desired terminal in the package model:
		terminal_cursor := terminal_properties (package_model_cursor, terminal_name);
		if terminal_cursor = pac_terminals.no_element then
			terminal_not_found (terminal_name);
		end if;

		-- Get the assembly technology of the terminal (SMT or THT):
		terminal_technology := element (terminal_cursor).technology;

		-- Get x/y of the terminal as given by the package model.
		-- This position is relative to the origin of the package model:
		terminal_position := to_vector (pac_terminals.element (terminal_cursor).position.place);
		
		-- Get the rotation of the terminal (about its center) as given by the package model:
		terminal_rotation := to_angle (pac_terminals.element (terminal_cursor).position.rotation);

		-- Add to the terminal rotation the rotation of the package:
		terminal_rotation := terminal_rotation + to_angle (get_rotation (package_position));

		
		-- In the board: If the package has been flipped (to any side) by the operator
		-- then the terminal must be flipped also.
		-- If the package has not been flipped, then we assume the face of the terminal 
		-- is the same as the face of the package.
		if element (device_cursor).flipped = YES then

			case terminal_technology is
				when SMT =>
					if element (terminal_cursor).face = TOP then
						terminal_position_face := BOTTOM;
					else
						terminal_position_face := TOP;
					end if;

				when THT => 
					-- If package flipped, then the face of the THT
					-- terminal is bottom. If package not flipped, then default TOP applies:
					terminal_position_face := BOTTOM;
			end case;

			
			-- mirror terminal position alog Y axis (swap right x with left x)
			mirror (terminal_position, Y);

			-- Rotate the terminal position (x/y) by the rotation of the package:
			rotate_by (terminal_position, - terminal_rotation);
			
		else -- not flipped
			terminal_position_face := get_face (package_position);

			-- Rotate the terminal position (x/y) by the rotation of the package:
			rotate_by (terminal_position, terminal_rotation);
		end if;


		-- Move the terminal position by the position of the package:
		move_by (terminal_position, to_offset (package_position.place));

		return (
			technology	=> terminal_technology,
			place		=> terminal_position,
			rotation	=> terminal_rotation,	   
			face		=> terminal_position_face);
		
	end get_terminal_position;


	function get_terminal_positions (
		module_cursor	: in pac_generic_modules.cursor;
		net_cursor		: in et_schematic.pac_nets.cursor)
		return pac_geometry_brd.pac_vectors.list
	is
		use pac_geometry_brd;
		use pac_vectors;
		result : pac_vectors.list;

		ports : et_nets.type_ports;

		port_properties : type_port_properties_access;

		use et_nets;
		use pac_device_ports;
		
		procedure query_device (d : in pac_device_ports.cursor) is
			device_cursor : pac_devices_sch.cursor;
			terminal_position : type_vector;
		begin
			device_cursor := locate_device (module_cursor, element (d).device_name);

			-- Only real devices have terminals. Virtual devices are ignored here:
			if is_real (device_cursor) then
				--put_line ("");
				--put_line ("dev  " & to_string (element (d).device_name));
				--put_line ("unit " & to_string (element (d).unit_name));
				--put_line ("port " & to_string (element (d).port_name));
				
				port_properties := get_port_properties (
					module_cursor	=> module_cursor,
					device_name		=> element (d).device_name,
					unit_name		=> element (d).unit_name,
					port_name		=> element (d).port_name);

				-- port_properties.terminal -- 14, H6

				-- Get for the candidate port the position of the associated terminal:
				terminal_position := 
					get_terminal_position (module_cursor, device_cursor, port_properties.terminal).place;
				
				-- Add the terminal position to the result:
				append (result, terminal_position);
			end if;
		end query_device;

		
		use pac_submodule_ports;
		procedure query_submodule (s : in pac_submodule_ports.cursor) is
		begin
			-- CS
			
			-- element (s).module_name
			-- element (s).port_name  -> position in brd
			null;
		end query_submodule;

		
		use et_netlists;
		use pac_netchanger_ports;
		procedure query_netchanger (n : in pac_netchanger_ports.cursor) is
		begin
			null;

			-- CS
			
			-- element (n).index
			-- element (n).port  -> position in brd
		end query_netchanger;
		
		
	begin
		-- Get the ports of devices, netchangers and submodules that are connected
		-- with the given net. Assume default assembly variant:
		ports := get_ports (
				net		=> net_cursor,
				variant	=> et_assembly_variants.pac_assembly_variants.no_element);


		iterate (ports.devices, query_device'access);
		iterate (ports.submodules, query_submodule'access);
		iterate (ports.netchangers, query_netchanger'access);
		
		return result;
	end get_terminal_positions;



	function get_all_terminals (
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return pac_terminals.map
	is
		use pac_packages_lib;
		package_model : constant pac_packages_lib.cursor := 
			get_package_model (device_cursor);
	begin
		return element (package_model).terminals;
	end get_all_terminals;
	
	
	function get_unconnected_terminals (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in et_schematic.pac_devices_sch.cursor) -- IC45
		return pac_terminals.map
	is
		use et_nets;
		use pac_terminals;
		result : pac_terminals.map;

		-- Get all terminals of the given device (according to its package variant):
		all_terminals : pac_terminals.map := get_all_terminals (device_cursor);

		-- Here we will store the terminals of the given device which are
		-- connected with nets:
		connected_terminals : pac_terminal_names.list;

		
		procedure query_net (net_cursor : in pac_nets.cursor) is
			-- Get the ports of all devices connected with the given net.
			-- Since this query is about the default assembly variant,
			-- we do not pass a specific assembly variant here.
			ports : constant type_ports := get_ports (net_cursor);

			use pac_device_ports;

			
			procedure query_device_port (d : in pac_device_ports.cursor) is

				port : type_device_port renames element (d);
				-- Now port contains the device name, unit name and port name.
				
				-- Get the cursor to the device in the schematic:
				device_cursor : constant pac_devices_sch.cursor := 
					locate_device (module_cursor, port.device_name);

				-- Get the cursor to the physical terminal (in the package model)
				-- that is linked with the port:
				terminal_cursor : constant pac_terminals.cursor := 
					get_terminal (device_cursor, port.unit_name, port.port_name);

				-- Get the terminal name (like 3 or H5):
				terminal_name : constant pac_terminal_name.bounded_string := 
					key (terminal_cursor);
				
			begin
				--put_line ("dev " & to_string (key (net_cursor)));
				if key (device_cursor) = key (get_unconnected_terminals.device_cursor) then
				
				-- Store the terminal name in list connected_terminals:
					connected_terminals.append (terminal_name);
				end if;
			end query_device_port;

			
		begin
			--put_line ("net " & to_string (key (net_cursor)));
			
			-- In variable "ports" we are interested in selector "devices" exclusively.
			-- Submodule ports and netchangers are just virtual devices
			-- that connect two conductor tracks. They can therefore be ignored:
			ports.devices.iterate (query_device_port'access);
		end query_net;

		
	begin
		--put_line ("device " & to_string (key (device_cursor)));
		--put_line ("all " & count_type'image (all_terminals.length));
		
		element (module_cursor).nets.iterate (query_net'access);

		--put_line ("connected " & count_type'image (connected_terminals.length));
		
		-- Remove the connected_terminals from all_terminals
		-- so that only the unconneced terminals are left:
		remove_terminals (all_terminals, connected_terminals);
		
		return result;
	end get_unconnected_terminals;
	



	function to_polygon (
		module_cursor	: in pac_generic_modules.cursor;
		device_cursor	: in pac_devices_sch.cursor;
		terminal_cursor	: in pac_terminals.cursor;
		layer_category	: in type_signal_layer_category;
		tolerance		: in type_distance_positive)
		return type_terminal_polygon
	is
		use pac_polygons;
		exists : boolean := false;
		result : type_polygon; -- to be returned


		use pac_terminals;
		
		-- Get the actual terminal as described in the package model:
		terminal : constant et_terminals.type_terminal := element (terminal_cursor);
		
		-- Get the terminal name (like 3 or H5):
		terminal_name : constant pac_terminal_name.bounded_string := key (terminal_cursor);
		
		-- Get the terminal position (incl. rotation and face):
		terminal_position : constant type_terminal_position_fine := 
			get_terminal_position (module_cursor, device_cursor, terminal_name);

		-- The displacement required to move the contour to 
		-- its final position:
		terminal_displacement : constant type_distance_relative := 
			to_distance_relative (terminal_position.place);

		
		-- intermediate place to a contour:
		contour : type_contour;


		-- Converts the contour to a polygon:
		procedure make_polygon is begin
			exists := true;
			
			result := to_polygon (
				contour		=> contour,
				tolerance	=> tolerance,
				mode		=> EXPAND, -- CS ?
				debug		=> false);
		end make_polygon;


		-- Mirrors the contour (if terminal is flipped to bottom side) and
		-- rotates the contour:
		procedure mirror_and_rotate is begin
			if terminal_position.face = BOTTOM then
				mirror (contour, Y);

				-- if on bottom side: rotate CW
				rotate_by (contour, - to_rotation (terminal_position.rotation));
			else
				-- if on top side: rotate CCW
				rotate_by (contour, + to_rotation (terminal_position.rotation));
			end if;
		end mirror_and_rotate;


		-- Moves the contour to the final position and converts it to a polygon.
		-- Optionally, if required by the caller, offsets the polygon edges
		-- by the width of the inner signal layer:
		procedure finalize (do_offset : in boolean := false) is
			use et_board_shapes_and_text.pac_polygon_offsetting;
		begin
			move_by (contour, terminal_displacement);
			make_polygon;
			if do_offset then
				offset_polygon (result, type_float_internal (terminal.width_inner_layers));
			end if;
		end finalize;
				
		
	begin -- to_polygon

		case terminal.technology is
			when THT => 
				case layer_category is
					when INNER =>								
						case terminal.tht_hole is
							when DRILLED =>
								contour := get_inner_contour (terminal, terminal_position.place);
								make_polygon;										
								
							when MILLED =>
								contour := terminal.millings;
								mirror_and_rotate;										
								finalize (do_offset => true);
								
						end case;
					
					when OUTER_TOP =>
						if terminal_position.face = TOP then
							contour := terminal.pad_shape_tht.top;
						else
							contour := terminal.pad_shape_tht.bottom;
						end if;
						mirror_and_rotate;
						finalize;

					when OUTER_BOTTOM =>
						if terminal_position.face = BOTTOM then
							contour := terminal.pad_shape_tht.top;
						else
							contour := terminal.pad_shape_tht.bottom;
						end if;
						mirror_and_rotate;
						finalize;
				end case;
				

			when SMT =>
				if layer_category = OUTER_TOP and terminal_position.face = TOP then
					contour := terminal.pad_shape_smt;
					rotate_by (contour, to_rotation (terminal_position.rotation));
					finalize;						
					
				elsif layer_category = OUTER_BOTTOM and terminal_position.face = BOTTOM then
					contour := terminal.pad_shape_smt;
					mirror (contour, Y);
					rotate_by (contour, - to_rotation (terminal_position.rotation));
					finalize;
				end if;
		end case;


		if exists then
			return (exists => TRUE, polygon => result);
		else
			return (exists => FALSE);
		end if;

	end to_polygon;

	
	
end et_board_ops.devices;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
