------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           BOARD OPERATIONS                               --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2019 Mario Blunk, Blunk electronic                 --
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

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.unbounded;
with ada.text_io;				use ada.text_io;

with ada.exceptions;
with ada.directories;
-- with gnat.directory_operations;

with ada.containers;            use ada.containers;
with ada.containers.doubly_linked_lists;
with ada.containers.ordered_maps;
-- with ada.containers.indefinite_ordered_sets;

with et_general;				use et_general;
-- with et_coordinates;
with et_string_processing;		use et_string_processing;
-- with et_libraries;				use et_libraries;
with et_schematic;				use et_schematic;
with et_pcb;
with et_pcb_coordinates;		use et_pcb_coordinates;
with et_project;				use et_project;
with schematic_ops;				use schematic_ops;
with assembly_variants;
with pick_and_place;
with submodules;
with numbering;
-- with conventions;
-- with material;
-- with netlists;
-- with et_geometry;

package body board_ops is
	
	procedure move_device (
	-- Moves a device in the board layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in et_pcb_coordinates.type_point_2d; -- x/y
		log_threshold	: in type_log_level) is

		use et_project.type_modules;
		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure set_position (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				case coordinates is
					when ABSOLUTE =>
						set_xy (point => device.position, position => point); -- preserve angle and face

					when RELATIVE =>
						move_point (point => device.position, offset => point); -- preserve angle and face
						
				end case;
			end;
			
		begin -- query_devices
			if contains (module.devices, device_name) then

				device_cursor := find (module.devices, device_name); -- the device should be there

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> set_position'access);

			else
				device_not_found (device_name);
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
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end move_device;

	procedure rotate_device (
	-- Rotates a device in the board layout.
	-- Leaves x/y and face (top/bottom) as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		coordinates		: in type_coordinates; -- relative/absolute		
		rotation		: in et_pcb_coordinates.type_angle; -- 90
		log_threshold	: in type_log_level) is

		use et_project.type_modules;
		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure set_rotation (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				case coordinates is
					when ABSOLUTE =>
						set_angle (point => device.position, value => rotation); -- preserve x/y and face

					when RELATIVE =>
						rotate (point => device.position, rotation => rotation); -- preserve x/y and face
				end case;
			end;
			
		begin -- query_devices
			if contains (module.devices, device_name) then

				device_cursor := find (module.devices, device_name); -- the device should be there

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> set_rotation'access);

			else
				device_not_found (device_name);
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
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end rotate_device;

	procedure flip_device (
	-- Flips a device in the board layout from top to bottom or vice versa.
	-- Leaves x/y and rotation as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		device_name		: in type_device_name; -- IC45
		face			: in type_face; -- top/bottom
		log_threshold	: in type_log_level) is

		use et_project.type_modules;
		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_devices (
			module_name	: in type_module_name.bounded_string;
			module		: in out type_module) is
			use et_schematic.type_devices;
			device_cursor : et_schematic.type_devices.cursor;

			procedure flip (
				device_name	: in type_device_name;
				device		: in out et_schematic.type_device) is
			begin
				set_face (position => device.position, face => face); -- preserve x/y and rotation
			end;
			
		begin -- query_devices
			if contains (module.devices, device_name) then

				device_cursor := find (module.devices, device_name); -- the device should be there

				-- set new position
				update_element (
					container	=> module.devices,
					position	=> device_cursor,
					process		=> flip'access);

			else
				device_not_found (device_name);
			end if;
		end query_devices;
		
	begin -- flip_device
		log (text => "module " & to_string (module_name) &
			" flipping device " & to_string (device_name) &
			" to" & to_string (face), level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_devices'access);

	end flip_device;

	function get_position (
	-- Returns the position (x/y/rotation) of a submodule instance.
	-- Assumptions:
	--  - The module to be searched in must be in the rig already.
	--  - The submodule instance must exist in the module.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		instance		: in et_general.type_module_instance_name.bounded_string) -- OSC1
		return et_pcb_coordinates.type_point_2d_with_angle is
		
		position : et_pcb_coordinates.type_point_2d_with_angle := submodule_position_default; -- to be returned

		module_cursor : type_modules.cursor; -- points to the module

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in et_schematic.type_module) is
			use submodules.type_submodules;
			submod_cursor : submodules.type_submodules.cursor;
		begin
			submod_cursor := find (module.submods, instance);
			position := element (submod_cursor).position_in_board;
		end;
		
	begin -- get_position
		-- locate the given module
		module_cursor := locate_module (module_name);

		type_modules.query_element (
			position	=> module_cursor,
			process		=> query_submodules'access);

		return position;
	end get_position;

	procedure move_submodule (
	-- Moves a submodule instance withing the parent module layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
		module_name		: in type_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in type_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in et_pcb_coordinates.type_point_2d; -- x/y
		log_threshold	: in type_log_level) is

		use et_project.type_modules;
		module_cursor : type_modules.cursor; -- points to the module being modified

		procedure query_submodules (
			module_name	: in type_module_name.bounded_string;
			module		: in out et_schematic.type_module) is
			use submodules.type_submodules;
			submod_cursor : submodules.type_submodules.cursor;

			procedure move (
				instance	: in et_general.type_module_instance_name.bounded_string;
				submodule	: in out submodules.type_submodule) is
			begin
				case coordinates is
					when ABSOLUTE =>
						set_xy (submodule.position_in_board, point);

					when RELATIVE =>
						move_point (submodule.position_in_board, point);
				end case;

				exception
					when event: others =>
						log (ERROR, "coordinates invalid !", console => true); -- CS required more details
						log (text => ada.exceptions.exception_information (event), console => true);
						raise;
				
			end move;

		begin -- query_submodules
			if contains (module.submods, instance) then

				submod_cursor := find (module.submods, instance); -- the submodule should be there

				update_element (
					container	=> module.submods,
					position	=> submod_cursor,
					process		=> move'access);

			else
				submodule_not_found (instance);
			end if;

		end;
		
	begin -- move_submodule
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving submodule instance " & to_string (instance) &
					" to" & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving submodule instance " & to_string (instance) &
					" by" & to_string (point), level => log_threshold);
		end case;

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

	end move_submodule;
	
	procedure make_pick_and_place (
	-- Exports a pick & place file from the given top module and assembly variant.
	-- CS: The rotation of submodules is currently ignored. The rotation defaults to zero degree.
	--     See comment in procedure query_submodules.
		module_name		: in type_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) is

		use et_project.type_modules;
		module_cursor : type_modules.cursor; -- points to the module

		use assembly_variants;
		use assembly_variants.type_variants;
		use et_general.type_variant_name;


		procedure make_for_variant (variant_name : in type_variant_name.bounded_string) is
			
			-- Here we collect the pick and place data in the first step. It will then
			-- be passed to procedure pick_and_place.write_pnp.
			use pick_and_place;
			pnp : pick_and_place.type_devices.map;

			procedure collect (
			-- Collects devices of the given module and its variant in container pnp.
			-- Adds to the device index the given offset.
			-- If offset is zero, we are dealing with the top module.
			-- The submodule position in the parent module is added to the device position.
				module_cursor		: in type_modules.cursor;
				variant				: in type_variant_name.bounded_string;
				offset				: in et_libraries.type_device_name_index;
				position_in_board	: in et_pcb_coordinates.type_point_2d_with_angle) -- submod pos. in parent
			is

				procedure log_position_in_board is begin
					if position_in_board /= submodule_position_default then
						log (text => "and applying submodule" & to_string (position_in_board),
							level => log_threshold + 1);
					end if;
				end;
				
				procedure query_devices (
					module_name	: in type_module_name.bounded_string;
					module		: in et_schematic.type_module) is

					device_name : et_libraries.type_device_name;
					inserted : boolean;

					function apply_position_in_board (position_generic : in type_package_position) return
						et_pcb_coordinates.type_package_position is 
						device_position : et_pcb_coordinates.type_package_position := position_generic;
					begin
						-- Get the device position in the generic submodule.
						-- Then move it according
						-- to the position of the submodule instance in the parent module:
						move_point (device_position, type_point_2d (position_in_board));

						log (text => "generic" & to_string (type_point_2d_with_angle (position_generic)) &
							" -> " & "in instance" & to_string (type_point_2d_with_angle (device_position)),
							level => log_threshold + 2);

						return device_position;
					end;
					
					procedure test_inserted is begin
						if not inserted then
							log (ERROR, "multiple occurence of device " & to_string (device_name),
									console => true);
							raise constraint_error;
						end if;
					end;

					procedure query_properties_default (cursor_schematic : in et_schematic.type_devices.cursor) is 
						cursor_pnp : pick_and_place.type_devices.cursor;

						use et_schematic.type_devices;
						use assembly_variants.type_devices;

					begin -- query_properties_default

						-- the device must be real (appearance SCH_PCB)
						if element (cursor_schematic).appearance = SCH_PCB then -- skip virtual devices

							-- the package of the device must be real
							if has_real_package (cursor_schematic) then
							
								device_name := et_schematic.type_devices.key (cursor_schematic);

								-- Store device in pnp list as it is:
								apply_offset (device_name, offset, log_threshold + 2);
								
								pick_and_place.type_devices.insert (
									container	=> pnp,
									key			=> device_name, -- IC4, R3
									new_item	=> (
										position	=> apply_position_in_board (element (cursor_schematic).position)),
		-- 								value		=> element (cursor_schematic).value,
		-- 								partcode	=> element (cursor_schematic).partcode,
		-- 								purpose		=> element (cursor_schematic).purpose,
		-- 								packge		=> et_schematic.package_model (cursor_schematic)),
									position	=> cursor_pnp,
									inserted	=> inserted);
								
								test_inserted;

							end if;
						end if;
					end query_properties_default;

					procedure query_properties_variants (cursor_schematic : in et_schematic.type_devices.cursor) is 
						cursor_pnp : pick_and_place.type_devices.cursor;

						use et_schematic.type_devices;
						alt_dev_cursor : assembly_variants.type_devices.cursor;
						use assembly_variants.type_devices;
					begin -- query_properties_variants

						-- the device must be real (appearance SCH_PCB)
						if element (cursor_schematic).appearance = SCH_PCB then -- skip virtual devices

							-- the package of the device must be real
							if has_real_package (cursor_schematic) then
													
								device_name := et_schematic.type_devices.key (cursor_schematic);
								
								-- Get a cursor to the alternative device as specified in the assembly variant:
								alt_dev_cursor := alternative_device (module_cursor, variant, device_name); 
								
								if alt_dev_cursor = assembly_variants.type_devices.no_element then
								-- Device has no entry in the assembly variant. -> It is to be stored in pnp list as it is:
								
									apply_offset (device_name, offset, log_threshold + 2);

									pick_and_place.type_devices.insert (
										container	=> pnp,
										key			=> device_name, -- IC4, R3
										new_item	=> (
											position	=> apply_position_in_board (element (cursor_schematic).position)),
	-- 										value		=> element (cursor_schematic).value,
	-- 										partcode	=> element (cursor_schematic).partcode,	
	-- 										purpose		=> element (cursor_schematic).purpose,
	-- 										packge		=> et_schematic.package_model (cursor_schematic)),
										position	=> cursor_pnp,
										inserted	=> inserted);

									test_inserted;

								else
								-- Device has an entry in the assembly variant. Depending on the mounted-flag
								-- it is to be skipped or inserted in pnp list with alternative properties.
								-- NOTE: The package model is not affected by the assembly variant.
									case element (alt_dev_cursor).mounted is
										when NO =>
											log (text => to_string (device_name) & " not mounted -> skipped",
												level => log_threshold + 2);
											
										when YES =>
											apply_offset (device_name, offset, log_threshold + 2);

											-- Insert the device in pnp list with alternative properties as defined
											-- in the assembly variant:
											pick_and_place.type_devices.insert (
												container	=> pnp,
												key			=> device_name, -- IC4, R3
												new_item	=> (
													position	=> apply_position_in_board (element (cursor_schematic).position)),
	-- 												value		=> element (alt_dev_cursor).value,
	-- 												partcode	=> element (alt_dev_cursor).partcode,
	-- 												purpose		=> element (alt_dev_cursor).purpose,
	-- 												packge		=> et_schematic.package_model (cursor_schematic)),
												position	=> cursor_pnp,
												inserted	=> inserted);

											test_inserted;

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
								et_libraries.to_string (offset), -- 100
							level => log_threshold + 1);

						log_position_in_board;

						log_indentation_up;
						
						et_schematic.type_devices.iterate (
							container	=> module.devices,
							process		=> query_properties_default'access);

					-- if a particular variant given, then collect devices accordingly:
					else
						log (text => "collecting devices from module " &
								enclose_in_quotes (to_string (module_name)) &
								" variant " & enclose_in_quotes (to_variant (variant)) &
								" by applying device index offset" & 
								et_libraries.to_string (offset), -- 100
							level => log_threshold + 1);

						log_position_in_board;

						log_indentation_up;
						
						et_schematic.type_devices.iterate (
							container	=> module.devices,
							process		=> query_properties_variants'access);

					end if;
					
					log_indentation_down;
				end query_devices;

			begin -- collect
				et_project.type_modules.query_element (
					position	=> module_cursor,
					process		=> query_devices'access);
				
			end collect;
	
			submod_tree : numbering.type_modules.tree := numbering.type_modules.empty_tree;
			tree_cursor : numbering.type_modules.cursor := numbering.type_modules.root (submod_tree);

			-- A stack keeps record of the submodule level where tree_cursor is pointing at.
			package stack_level is new et_general.stack_lifo (
				item	=> numbering.type_modules.cursor,
				max 	=> submodules.nesting_depth_max);

			-- Another stack keeps record of the assembly variant on submodule levels.
			package stack_variant is new et_general.stack_lifo (
				item	=> type_variant_name.bounded_string,
				max 	=> submodules.nesting_depth_max);
			
			variant : type_variant_name.bounded_string; -- low_cost

			-- Another stack keeps record of the submodule position (inside the parent module) on submodule levels.
			package stack_position_in_board is new et_general.stack_lifo (
				item	=> et_pcb_coordinates.type_point_2d_with_angle,
				max 	=> submodules.nesting_depth_max);

			-- This is the position of the submodule in the board (usually its lower left corner):
			position_in_board : et_pcb_coordinates.type_point_2d_with_angle := submodule_position_default;
			
			procedure query_submodules is 
			-- Reads the submodule tree submod_tree. It is recursive, means it calls itself
			-- until the deepest submodule (the bottom of the design structure) has been reached.
				use numbering.type_modules;
				module_name 	: type_module_name.bounded_string; -- motor_driver
				parent_name 	: type_module_name.bounded_string; -- water_pump
				module_instance	: et_general.type_module_instance_name.bounded_string; -- MOT_DRV_3
				offset			: et_libraries.type_device_name_index;

				use assembly_variants.type_submodules;
				alt_submod : assembly_variants.type_submodules.cursor;
			begin
				log_indentation_up;

				-- start with the first submodule on the current hierarchy level
				tree_cursor := first_child (tree_cursor);

				-- iterate through the submodules on this level
				while tree_cursor /= numbering.type_modules.no_element loop
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
						parent_name := make_pick_and_place.module_name;
						variant := variant_name; -- argument of make_bom
					else
						parent_name := element (parent (tree_cursor)).name;
					end if;

					-- Get the device name offset of the current submodule;
					offset := element (tree_cursor).device_names_offset;

					if not is_default (variant) then
						-- Query in parent module: Is there any assembly variant specified for this submodule ?

						alt_submod := alternative_submodule (
									module	=> locate_module (parent_name),
									variant	=> variant,
									submod	=> module_instance);

						if alt_submod = assembly_variants.type_submodules.no_element then
						-- no variant specified for this submodule -> collect devices of default variant

							variant := default;
						else
						-- alternative variant specified for this submodule
							variant := element (alt_submod).variant;
						end if;

					end if;

					-- backup the position_in_board of this submodule
					stack_position_in_board.push (position_in_board);

					-- The new position_in_board is a vector sum of the position_in_board of the parent module
					-- and the position_in_board of the current submodule:
					move_point (position_in_board, type_point_2d (get_position (parent_name, module_instance)));

					-- CS position_in_board must be rotated according to rotation specified where
					-- the submodule has been instanciated. 
					
					-- collect devices from current module
					collect (
						module_cursor		=> locate_module (module_name),
						variant				=> variant,
						offset				=> offset,
						position_in_board	=> position_in_board -- the position of the submodule inside the parent module
						);

					if first_child (tree_cursor) = numbering.type_modules.no_element then 
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

					-- restore the position_in_board of this submodule
					position_in_board := stack_position_in_board.pop;
					
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

			-- Collect devices of the given top module.
			-- NOTE: The top module has no device index offset and
			-- zero relative position to anywhere because it is not
			-- encapsulated in any parent module.
			collect (
				module_cursor		=> module_cursor,
				variant				=> variant_name,
				offset				=> 0,
				position_in_board	=> submodule_position_default -- zero x/x/rotation
				); 
			
			-- take a copy of the submodule tree of the given top module:
			submod_tree := element (module_cursor).submod_tree;

			-- set the cursor inside the tree at root position:
			tree_cursor := numbering.type_modules.root (submod_tree);
			
			stack_level.init;
			stack_variant.init;
			stack_position_in_board.init;
			
			-- collect devices of the submodules
			query_submodules;

			-- write the pick and place file
			pick_and_place.write_pnp (
				pnp				=> pnp,				-- the container that holds the pick and place list
				module_name		=> module_name,		-- motor_driver
				variant_name	=> variant_name,	-- low_cost
				-- format	=> NATIVE			-- CS: should be an argument in the future
				log_threshold	=> log_threshold + 1);
			
			log_indentation_down;
		end make_for_variant;
		
		procedure query_variant (variant_cursor : in type_variants.cursor) is
			use et_general.type_variant_name;
		begin
			make_for_variant (key (variant_cursor));
		end query_variant;
		
	begin -- make_pick_and_place
		log (text => "generating pick & place data ...", level => log_threshold);
		log_indentation_up;
		
		-- locate the given top module
		module_cursor := locate_module (module_name);

		-- Build the submodule tree of the module according to the current design structure.
		-- All further operations rely on this tree:
		schematic_ops.build_submodules_tree (
			module_name 	=> module_name,
			log_threshold	=> log_threshold + 1);

		-- make p&p of default variant
		make_for_variant (default);

		-- make p&p of other variants
		iterate (element (module_cursor).variants, query_variant'access);
		
		log_indentation_down;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
		
	end make_pick_and_place;

	
	function terminal_position (
	-- Returns the coordinates of a terminal.
		module_cursor	: in et_project.type_modules.cursor;
		device_cursor	: in et_schematic.type_devices.cursor; -- IC45
		terminal_cursor	: in et_pcb.type_terminals.cursor) -- H7, 14
		return type_terminal_position is
		use et_pcb;
		pos : type_terminal_position (SMT); -- to be returned

		model : type_package_model_file.bounded_string; -- libraries/packages/smd/SOT23.pac
		model_cursor : type_packages.cursor;
	begin
		-- get the package model of the given device:
		model := package_model (device_cursor);

		-- set cursor to package model:
		model_cursor := locate_package_model (model);
		
		return pos; -- CS
	end terminal_position;
	
end board_ops;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
