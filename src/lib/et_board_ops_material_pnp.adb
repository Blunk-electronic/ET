------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--               BOARD OPERATIONS / MATERIAL / PICK AND PLACE               --
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
-- To Do: 
-- - clean up, rework
--

with ada.text_io;							use ada.text_io;
with ada.strings;							use ada.strings;
with ada.exceptions;						use ada.exceptions;

with et_string_processing;					use et_string_processing;

with et_pick_and_place;
with et_devices_electrical;					use et_devices_electrical;
-- CS export non electrical devices missing ?

with et_numbering;
with et_symbol_model;
with et_device_appearance;
with et_device_name;
with et_generic_stacks;

with et_assembly_variants;
with et_schematic_ops_assembly_variant;		use et_schematic_ops_assembly_variant;

with et_module;								use et_module;
with et_module_instance;					use et_module_instance;

with et_submodules;
with et_schematic_ops_submodules;
with et_board_ops_submodule;				use et_board_ops_submodule;


package body et_board_ops_material_pnp is


	use pac_devices_electrical;
	

	
	procedure make_pick_and_place (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

		use et_device_name;
		use et_assembly_variants;
		use et_assembly_variants.pac_assembly_variants;
		use pac_assembly_variant_name;

		procedure make_for_variant (variant_name : in pac_assembly_variant_name.bounded_string) is
			
			-- Here we collect the pick and place data in the first step. It will then
			-- be passed to procedure et_pick_and_place.write_pnp.
			use et_pick_and_place;
			pnp : et_pick_and_place.pac_devices.map;

			
			procedure collect (
			-- Collects devices of the given module and its variant in container pnp.
			-- Adds to the device index the given offset.
			-- If offset is zero, we are dealing with the top module.
			-- The submodule position in the parent module is added to the device position.
				module_cursor		: in pac_generic_modules.cursor;
				variant				: in pac_assembly_variant_name.bounded_string;
				offset				: in type_name_index;
				position_in_board	: in type_position) -- submod pos. in parent
			is

				procedure log_position_in_board is begin
					if position_in_board /= origin_zero_rotation then
						log (text => "and applying submodule" & to_string (position_in_board),
							level => log_threshold + 1);
					end if;
				end;

				
				procedure query_devices (
					module_name	: in pac_module_name.bounded_string;
					module		: in type_generic_module) 
				is
					device_name : type_device_name;
					inserted : boolean;

					function apply_position_in_board (position_generic : in type_package_position) return
						et_board_coordinates.type_package_position 
					is 
						-- Get the device position in the generic submodule:.
						device_position : et_board_coordinates.type_package_position := position_generic;
					begin
						-- Then move it according
						-- to the position of the submodule instance in the parent module:
						--move_by (device_position, to_distance_relative (position_in_board));
						move_by (device_position.place, position_in_board.place);

						log (text => "generic" & to_string (position_generic) &
							" -> " & "in instance" & to_string (device_position),
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

					
					procedure query_properties_default (cursor_schematic : in pac_devices_electrical.cursor) is 
						cursor_pnp : et_pick_and_place.pac_devices.cursor;

						use et_assembly_variants.pac_device_variants;
						use et_symbol_model;
						use et_device_appearance;

					begin -- query_properties_default

						-- the device must be real (appearance PCB)
						if element (cursor_schematic).appearance = APPEARANCE_PCB then -- skip virtual devices

							-- the package of the device must be real
							if is_bom_relevant (cursor_schematic) then
							
								device_name := pac_devices_electrical.key (cursor_schematic);

								-- Store device in pnp list as it is:
								apply_offset (device_name, offset, log_threshold + 2);
								
								et_pick_and_place.pac_devices.insert (
									container	=> pnp,
									key			=> device_name, -- IC4, R3
									new_item	=> (
										position	=> apply_position_in_board (element (cursor_schematic).position)),
		-- 								value		=> element (cursor_schematic).value,
		-- 								partcode	=> element (cursor_schematic).partcode,
		-- 								purpose		=> element (cursor_schematic).purpose,
		-- 								packge		=> package_model (cursor_schematic)),
									position	=> cursor_pnp,
									inserted	=> inserted);
								
								test_inserted;

							end if;
						end if;
					end query_properties_default;


					
					procedure query_properties_variants (
						cursor_schematic : in pac_devices_electrical.cursor) 
					is 
						cursor_pnp : et_pick_and_place.pac_devices.cursor;

						alt_dev_cursor : et_assembly_variants.pac_device_variants.cursor;
						use et_assembly_variants.pac_device_variants;
						use et_symbol_model;
						use et_device_appearance;
						
					begin -- query_properties_variants

						-- the device must be real (appearance PCB)
						if element (cursor_schematic).appearance = APPEARANCE_PCB then -- skip virtual devices

							-- the package of the device must be real
							if is_bom_relevant (cursor_schematic) then
													
								device_name := pac_devices_electrical.key (cursor_schematic);
								
								-- Get a cursor to the alternative device as specified in the assembly variant:
								alt_dev_cursor := get_alternative_device (module_cursor, variant, device_name); 
								
								if alt_dev_cursor = et_assembly_variants.pac_device_variants.no_element then
								-- Device has no entry in the assembly variant. -> It is to be stored in pnp list as it is:
								
									apply_offset (device_name, offset, log_threshold + 2);

									et_pick_and_place.pac_devices.insert (
										container	=> pnp,
										key			=> device_name, -- IC4, R3
										new_item	=> (
											position	=> apply_position_in_board (element (cursor_schematic).position)),
	-- 										value		=> element (cursor_schematic).value,
	-- 										partcode	=> element (cursor_schematic).partcode,	
	-- 										purpose		=> element (cursor_schematic).purpose,
	-- 										packge		=> package_model (cursor_schematic)),
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
											et_pick_and_place.pac_devices.insert (
												container	=> pnp,
												key			=> device_name, -- IC4, R3
												new_item	=> (
													position	=> apply_position_in_board (element (cursor_schematic).position)),
	-- 												value		=> element (alt_dev_cursor).value,
	-- 												partcode	=> element (alt_dev_cursor).partcode,
	-- 												purpose		=> element (alt_dev_cursor).purpose,
	-- 												packge		=> package_model (cursor_schematic)),
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
								to_string (offset), -- 100
							level => log_threshold + 1);

						log_position_in_board;

						log_indentation_up;
						
						pac_devices_electrical.iterate (
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

						log_position_in_board;

						log_indentation_up;
						
						pac_devices_electrical.iterate (
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

			-- Another stack keeps record of the submodule position (inside the parent module) on submodule levels.
			package stack_position_in_board is new et_generic_stacks.stack_lifo (
				item	=> type_position,
				max 	=> et_submodules.nesting_depth_max);

			-- This is the position of the submodule in the board (usually its lower left corner):
			position_in_board : type_position := origin_zero_rotation;

			
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

				use et_schematic_ops_submodules;
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
						parent_name := make_pick_and_place.module_name;
						variant := variant_name; -- argument of make_bom
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

					-- backup the position_in_board of this submodule
					stack_position_in_board.push (position_in_board);

					-- The new position_in_board is a vector sum of the position_in_board of the parent module
					-- and the position_in_board of the current submodule:
					--move_by (position_in_board, to_distance_relative (get_position (parent_name, module_instance)));
					move_by (position_in_board.place, get_position (parent_name, module_instance).place);
					
					-- CS position_in_board must be rotated according to rotation specified where
					-- the submodule has been instanciated. 
					
					-- collect devices from current module
					collect (
						module_cursor		=> locate_module (module_name),
						variant				=> variant,
						offset				=> offset,
						position_in_board	=> position_in_board -- the position of the submodule inside the parent module
						);

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
				position_in_board	=> origin_zero_rotation -- zero x/x/rotation
				); 
			
			-- take a copy of the submodule tree of the given top module:
			submod_tree := element (module_cursor).submod_tree;

			-- set the cursor inside the tree at root position:
			tree_cursor := et_numbering.pac_modules.root (submod_tree);
			
			stack_level.init;
			stack_variant.init;
			stack_position_in_board.init;
			
			-- collect devices of the submodules
			query_submodules;

			-- write the pick and place file
			et_pick_and_place.write_pnp (
				pnp				=> pnp,				-- the container that holds the pick and place list
				module_name		=> module_name,		-- motor_driver
				variant_name	=> variant_name,	-- low_cost
				-- format	=> NATIVE			-- CS: should be an argument in the future
				log_threshold	=> log_threshold + 1);
			
			log_indentation_down;
		end make_for_variant;

		
		procedure query_variant (variant_cursor : in et_assembly_variants.pac_assembly_variants.cursor) is
			use pac_assembly_variant_name;
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
		et_schematic_ops_submodules.build_submodules_tree (
			module_name 	=> module_name,
			log_threshold	=> log_threshold + 1);

		-- make p&p of default variant
		make_for_variant (default);

		-- make p&p of other variants
		iterate (element (module_cursor).assembly_variants.variants, query_variant'access);
		
		log_indentation_down;

		exception
			when event: others =>
				log_indentation_reset;
				log (text => ada.exceptions.exception_information (event), console => true);
				raise;
		
	end make_pick_and_place;

	

end et_board_ops_material_pnp;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
