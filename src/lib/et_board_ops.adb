------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           BOARD OPERATIONS                               --
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



with et_netlists;
with et_device_query_schematic;		use et_device_query_schematic;
with et_schematic_ops.nets;			use et_schematic_ops.nets;
with et_schematic_ops;				use et_schematic_ops;

with et_submodules;
with et_numbering;
with et_symbols;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;			use et_fill_zones.boards;
with et_exceptions;					use et_exceptions;

with et_routing;


package body et_board_ops is

	use pac_generic_modules;

	use pac_devices_sch;
	use pac_devices_non_electric;
	use pac_nets;

	

	
	procedure no_segment_found (
		point		: in type_point; 
		accuracy	: in type_catch_zone)
	is begin
		log (importance => WARNING, 
			 text => "nothing found at" & to_string (point) &
			 " in vicinity of" & catch_zone_to_string (accuracy));
	end no_segment_found;

	

	
	procedure move_board (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure set_origin (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
		begin
			case coordinates is
				when ABSOLUTE =>
					module.board.origin := point;

				when RELATIVE =>
					move_by (module.board.origin, to_distance_relative (point));
			end case;
		end set_origin;
		
	begin -- move_board
		case coordinates is
			when ABSOLUTE =>
				log (text => "module " & to_string (module_name) &
					" moving board origin to" & to_string (point), level => log_threshold);

			when RELATIVE =>
				log (text => "module " & to_string (module_name) &
					" moving board origin by" & to_string (point), level => log_threshold);
		end case;

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> set_origin'access);

	end move_board;

	
	procedure add_layer (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		layer			: in et_pcb_stack.type_layer; -- incl. conductor and dieelectic thickness
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use et_geometry;
		
		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use et_pcb_stack.package_layers;
		begin
			append (module.board.stack.layers, layer);
		end add;
		
	begin -- add_layer
		log (text => "module " & to_string (module_name) &
			" adding layer conductor thickness" & to_string (layer.conductor.thickness) &
			" dielectic thickness" & to_string (layer.dielectric.thickness),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);
		
	end add_layer;

	
	function layer_count (module_cursor	: in et_project.modules.pac_generic_modules.cursor) 
		return et_pcb_stack.type_signal_layer 
	is
		use package_layers;
	begin
		return last_index (element (module_cursor).board.stack.layers) + 1;
	end;

	
	procedure test_layer (
		module_cursor	: in et_project.modules.pac_generic_modules.cursor;
		layer			: in et_pcb_stack.type_signal_layer) 
	is
		layers_used : et_pcb_stack.type_signal_layer := layer_count (module_cursor);
	begin
		if layer > layers_used then
			log (ERROR, "Layer " & to_string (layer) & " invalid !" &
				 " The current layer stack allows only " & to_string (layers_used) & " layers !",
				 console => true);
			raise constraint_error;
		end if;
	end;

	
	procedure delete_layer (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		layer			: in et_pcb_stack.type_signal_layer;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use package_layers;

			-- get the total number of layers used by the module
			layers_used : type_signal_layer := layer_count (module_cursor);

			old_stack : package_layers.vector := element (module_cursor).board.stack.layers;
			new_stack : package_layers.vector;
		begin
			-- The bottom layer can not be deleted:
			if layer = layers_used then
				log (WARNING, "The bottom layer" & to_string (layer) & " can not be deleted !");

			-- The layer must not be greater than the total number of layers:
			elsif layer > layers_used then
				log (WARNING, "layer" & to_string (layer) & " does not exist. " &
					 "The board uses only" & to_string (layers_used) & " layers !");
			else
				-- Rebuild the layer stack by copying the old layers one by one
				-- to the new layer stack. The layer to be deleted is skipped:
				for i in first_index (old_stack) .. last_index (old_stack) loop
					if i /= layer then
						append (new_stack, element (old_stack, i));
					end if;
				end loop;

				module.board.stack.layers := new_stack;
			end if;
		end delete;

		
	begin -- delete_layer
		log (text => "module " & to_string (module_name) &
			" deleting layer" & to_string (layer),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_layer;
	


	
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

	
	-- Moves a submodule instance within the parent module layout in x/y direction.
	-- Leaves rotation and face (top/bottom) as it is.
	procedure move_submodule (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		instance		: in pac_module_instance_name.bounded_string; -- OSC1
		coordinates		: in type_coordinates; -- relative/absolute		
		point			: in type_point; -- x/y
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use et_submodules.pac_submodules;
			submod_cursor : et_submodules.pac_submodules.cursor;

			procedure move (
				instance	: in et_general.pac_module_instance_name.bounded_string;
				submodule	: in out et_submodules.type_submodule) 
			is begin
				case coordinates is
					when ABSOLUTE =>
						set (submodule.position_in_board, point);

					when RELATIVE =>
						move_by (submodule.position_in_board.place, to_distance_relative (point));
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
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> query_submodules'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end move_submodule;

	
	procedure make_pick_and_place (
		module_name		: in pac_module_name.bounded_string; -- the parent module like motor_driver (without extension *.mod)
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module

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
					module		: in type_module) 
				is
					device_name : type_device_name;
					inserted : boolean;

					function apply_position_in_board (position_generic : in type_package_position) return
						et_pcb_coordinates.type_package_position 
					is 
						-- Get the device position in the generic submodule:.
						device_position : et_pcb_coordinates.type_package_position := position_generic;
					begin
						-- Then move it according
						-- to the position of the submodule instance in the parent module:
						--move_by (device_position, to_distance_relative (position_in_board));
						move_by (device_position.place, to_distance_relative (position_in_board.place));

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

					
					procedure query_properties_default (cursor_schematic : in pac_devices_sch.cursor) is 
						cursor_pnp : et_pick_and_place.pac_devices.cursor;

						use et_assembly_variants.pac_device_variants;
						use et_symbols;

					begin -- query_properties_default

						-- the device must be real (appearance PCB)
						if element (cursor_schematic).appearance = PCB then -- skip virtual devices

							-- the package of the device must be real
							if has_real_package (cursor_schematic) then
							
								device_name := pac_devices_sch.key (cursor_schematic);

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

					
					procedure query_properties_variants (cursor_schematic : in pac_devices_sch.cursor) is 
						cursor_pnp : et_pick_and_place.pac_devices.cursor;

						alt_dev_cursor : et_assembly_variants.pac_device_variants.cursor;
						use et_assembly_variants.pac_device_variants;
						use et_symbols;
						
					begin -- query_properties_variants

						-- the device must be real (appearance PCB)
						if element (cursor_schematic).appearance = PCB then -- skip virtual devices

							-- the package of the device must be real
							if has_real_package (cursor_schematic) then
													
								device_name := pac_devices_sch.key (cursor_schematic);
								
								-- Get a cursor to the alternative device as specified in the assembly variant:
								alt_dev_cursor := alternative_device (module_cursor, variant, device_name); 
								
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
								et_devices.to_string (offset), -- 100
							level => log_threshold + 1);

						log_position_in_board;

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
								et_devices.to_string (offset), -- 100
							level => log_threshold + 1);

						log_position_in_board;

						log_indentation_up;
						
						pac_devices_sch.iterate (
							container	=> module.devices,
							process		=> query_properties_variants'access);

					end if;
					
					log_indentation_down;
				end query_devices;

				
			begin -- collect
				et_project.modules.pac_generic_modules.query_element (
					position	=> module_cursor,
					process		=> query_devices'access);
				
			end collect;

			
			submod_tree : et_numbering.pac_modules.tree := et_numbering.pac_modules.empty_tree;
			tree_cursor : et_numbering.pac_modules.cursor := et_numbering.pac_modules.root (submod_tree);

			
			-- A stack keeps record of the submodule level where tree_cursor is pointing at.
			package stack_level is new et_general.stack_lifo (
				item	=> et_numbering.pac_modules.cursor,
				max 	=> et_submodules.nesting_depth_max);

			-- Another stack keeps record of the assembly variant on submodule levels.
			package stack_variant is new et_general.stack_lifo (
				item	=> pac_assembly_variant_name.bounded_string,
				max 	=> et_submodules.nesting_depth_max);
			
			variant : pac_assembly_variant_name.bounded_string; -- low_cost

			-- Another stack keeps record of the submodule position (inside the parent module) on submodule levels.
			package stack_position_in_board is new et_general.stack_lifo (
				item	=> type_position,
				max 	=> et_submodules.nesting_depth_max);

			-- This is the position of the submodule in the board (usually its lower left corner):
			position_in_board : type_position := origin_zero_rotation;

			
			procedure query_submodules is 
			-- Reads the submodule tree submod_tree. It is recursive, means it calls itself
			-- until the deepest submodule (the bottom of the design structure) has been reached.
				use et_numbering.pac_modules;
				module_name 	: pac_module_name.bounded_string; -- motor_driver
				parent_name 	: pac_module_name.bounded_string; -- water_pump
				module_instance	: et_general.pac_module_instance_name.bounded_string; -- MOT_DRV_3
				offset			: et_devices.type_name_index;

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
					move_by (position_in_board.place, to_distance_relative (get_position (parent_name, module_instance).place));
					
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
		et_schematic_ops.build_submodules_tree (
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

	


	function get_via_positions (
		net_cursor : in et_schematic.pac_nets.cursor)
		return pac_points.list
	is
		use pac_points;
		result : pac_points.list;

		use pac_vias;
		procedure query_via (v : in pac_vias.cursor) is begin
			append (result, element (v).position);
		end query_via;
		
	begin
		iterate (element (net_cursor).route.vias, query_via'access);
		return result;
	end get_via_positions;
	

	function get_track_ends (
		net_cursor : in et_schematic.pac_nets.cursor)
		return pac_points.list
	is
		use pac_points;
		result : pac_points.list;

		use pac_conductor_lines;
		procedure query_line (l : in pac_conductor_lines.cursor) is
		begin
			append (result, element (l).start_point);
			append (result, element (l).end_point);
		end query_line;

		
		use pac_conductor_arcs;
		procedure query_arc (a : in pac_conductor_arcs.cursor) is
		begin
			append (result, element (a).start_point);
			append (result, element (a).end_point);
		end query_arc;

		
	begin
		-- Query lines and arc segments:
		iterate (element (net_cursor).route.lines, query_line'access);
		iterate (element (net_cursor).route.arcs, query_arc'access);

		-- The ends of segments frequently overlap with those of other
		-- segments. This causes redundant points which must be removed:
		remove_redundant_points (result);
		
		return result;
	end get_track_ends;


	
	procedure set_grid (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		grid			: in type_grid;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is begin
			module.board.grid := grid;
		end;
		
	begin -- set_grid
		log (text => "module " & enclose_in_quotes (to_string (module_name))
			& " setting board grid" & to_string (grid),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

	end set_grid;
	

	procedure set_grid (
		module_cursor	: in pac_generic_modules.cursor;
		grid			: in type_grid;
		log_threshold	: in type_log_level) 
	is
		
		procedure do_it (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is begin
			module.board.grid := grid;
		end;
		
	begin -- set_grid
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor)))
			& " setting board grid" & to_string (grid),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> do_it'access);

	end set_grid;

	

	
	

	


	procedure update_ratsnest (
		module_cursor	: in pac_generic_modules.cursor;
		lth				: in type_log_level)
	is separate;
	

	
	procedure place_via (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in pac_net_name.bounded_string; -- reset_n
		via				: in type_via;
		log_threshold	: in type_log_level) 
	is
		console : boolean := false; -- for test and debugging only

		procedure locate_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			net_cursor : pac_nets.cursor := find (module.nets, net_name);

			use et_nets;
			
			procedure locate_net (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net) 
			is
				use pac_vias;
			begin
				append (
					container	=> net.route.vias,
					new_item	=> via);
			end locate_net;
			
		begin -- locate_module
			if net_exists (net_cursor) then

				pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> locate_net'access);
				
			else
				net_not_found (net_name);
			end if;
		end locate_module;
						  
	begin -- place_via
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " placing via in net " & to_string (net_name) 
			& " at" & to_string (via.position)
			& " drill size " & to_string (via.diameter)
			& " cat " & to_string (via.category),
			console => console,
			level => log_threshold);

		case via.category is
			when THROUGH =>
				log (text => keyword_restring_inner & space
					 & to_string (via.restring_inner), 
					console => console,
					level => log_threshold);

				log (text => keyword_restring_outer & space
					 & to_string (via.restring_outer),
					console => console,
					level => log_threshold);

				
			when BLIND_DRILLED_FROM_TOP =>
				log (text => keyword_destination & space
					 & to_string (via.lower),
					console => console,
					level => log_threshold);

				log (text => keyword_restring_inner & space
					 & to_string (via.restring_inner),
					console => console,
					level => log_threshold);
				
				log (text => keyword_restring_outer & space
					 & to_string (via.restring_top), 
					console => console,
					level => log_threshold);


			when BLIND_DRILLED_FROM_BOTTOM =>
				log (text => keyword_destination & space
					 & to_string (via.upper), 
					console => console,
					level => log_threshold);

				log (text => keyword_restring_inner & space
					 & to_string (via.restring_inner),
					console => console,
					level => log_threshold);
				
				log (text => keyword_restring_outer & space
					 & to_string (via.restring_bottom),
					console => console,
					level => log_threshold);

				
			when BURIED =>
				log (text => et_vias.keyword_layers & space
					 & to_string (via.layers),
					console => console,
					level => log_threshold);

				log (text => keyword_restring_inner & space
					 & to_string (via.restring_inner),
					console => console,
					level => log_threshold);

		end case;

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> locate_module'access);

		update_ratsnest (module_cursor, log_threshold + 1);
	end place_via;
		
	
	
	
-- ROUTE RESTRICT

	procedure test_layers (
	-- Tests the given set of signal layers whether each of them is available
	-- according to the current layer stack of the given module.
		module_cursor	: in et_project.modules.pac_generic_modules.cursor;
		layers 			: in et_pcb_stack.type_signal_layers.set)
	is
		use type_signal_layers;
		
		procedure query_layer (cursor : in type_signal_layers.cursor) is
		begin
			test_layer (module_cursor, element (cursor));
		end;
		
	begin
		iterate (layers, query_layer'access);
	end;

	
	procedure draw_route_restrict_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		line			: in type_route_restrict_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_route_restrict_lines;

		procedure draw (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
		begin
			append (
				container	=> module.board.route_restrict.lines,
				new_item	=> line);
		end;
		
	begin 
		log (text => "module " & to_string (module_name) &
			" drawing route restrict line in layer(s) " & to_string (line.layers) &
			to_string (line),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layers are available according to current layer stack:
		test_layers (module_cursor, line.layers);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> draw'access);
		
	end draw_route_restrict_line;

	
	procedure draw_route_restrict_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		arc				: in type_route_restrict_arc;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_route_restrict_arcs;

		procedure draw (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
		begin
			append (
				container	=> module.board.route_restrict.arcs,
				new_item	=> arc);
		end;
		
	begin 
		log (text => "module " & to_string (module_name) &
			" drawing route restrict arc in layer(s) " & to_string (arc.layers) &
			to_string (arc),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layers are available according to current layer stack:
		test_layers (module_cursor, arc.layers);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> draw'access);
		
	end draw_route_restrict_arc;

	
	procedure draw_route_restrict_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		circle			: in type_route_restrict_circle;
		log_threshold	: in type_log_level) is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_route_restrict_circles;

		procedure draw (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) is
		begin
			append (
				container	=> module.board.route_restrict.circles,
				new_item	=> circle);
		end;
		
	begin 
		log (text => "module " & to_string (module_name) &
			" drawing route restrict circle in layer(s) " & to_string (circle.layers) &
			to_string (circle),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layers are available according to current layer stack:
		test_layers (module_cursor, circle.layers);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> draw'access);
		
	end draw_route_restrict_circle;

	
	procedure delete_route_restrict (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is
			use pac_route_restrict_lines;
			use pac_route_restrict_arcs;
			use pac_route_restrict_circles;
			line_cursor   : pac_route_restrict_lines.cursor  := module.board.route_restrict.lines.first;
			arc_cursor    : pac_route_restrict_arcs.cursor   := module.board.route_restrict.arcs.first;
			circle_cursor : pac_route_restrict_circles.cursor := module.board.route_restrict.circles.first;

			deleted : boolean := false; -- goes true if at least one segment has been deleted
		begin
			-- first search for a matching segment among the lines
			while line_cursor /= pac_route_restrict_lines.no_element loop
				if element (line_cursor).on_line (point) then
					delete (module.board.route_restrict.lines, line_cursor);
					deleted := true;
					exit;
				end if;
				next (line_cursor);
			end loop;

			-- if no line found, search among arcs
			if not deleted then
				while arc_cursor /= pac_route_restrict_arcs.no_element loop
					
					if element (arc_cursor).on_arc (point) then
						delete (module.board.route_restrict.arcs, arc_cursor);
						deleted := true;
						exit;
					end if;
					
					next (arc_cursor);
				end loop;
			end if;

			-- if no arc found, search among circles
			if not deleted then
				while circle_cursor /= pac_route_restrict_circles.no_element loop
					
					if element (circle_cursor).on_circle (point) then
						delete (module.board.route_restrict.circles, circle_cursor);
						deleted := true;
						exit;
					end if;
					
					next (circle_cursor);
				end loop;
			end if;

			if not deleted then
				no_segment_found (point, accuracy);
			end if;
			
		end delete;

		
	begin -- delete_route_restrict
		log (text => "module " & to_string (module_name) &
			" deleting route restrict segment" &
			" at" & to_string (point) &
			" accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_route_restrict;

	
	
-- VIA RESTRICT

	procedure draw_via_restrict_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		line			: in type_via_restrict_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_via_restrict_lines;

		procedure draw (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is begin
			append (
				container	=> module.board.via_restrict.lines,
				new_item	=> line);
		end;
		
	begin 
		log (text => "module " & to_string (module_name) &
			" drawing via restrict line in layer(s) " & to_string (line.layers) &
			to_string (line),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layers are available according to current layer stack:
		test_layers (module_cursor, line.layers);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> draw'access);
		
	end draw_via_restrict_line;

	
	procedure draw_via_restrict_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		arc				: in type_via_restrict_arc;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_via_restrict_arcs;

		procedure draw (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is begin
			append (
				container	=> module.board.via_restrict.arcs,
				new_item	=> arc);
		end;
		
	begin 
		log (text => "module " & to_string (module_name) &
			" drawing via restrict arc in layer(s) " & to_string (arc.layers) &
			to_string (arc),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layers are available according to current layer stack:
		test_layers (module_cursor, arc.layers);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> draw'access);
		
	end draw_via_restrict_arc;

	
	procedure draw_via_restrict_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		circle			: in type_via_restrict_circle;
		log_threshold	: in type_log_level) 
	is

		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		use pac_via_restrict_circles;

		procedure draw (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is begin
			append (
				container	=> module.board.via_restrict.circles,
				new_item	=> circle);
		end;
		
	begin 
		log (text => "module " & to_string (module_name) &
			" drawing via restrict circle in layer(s) " & to_string (circle.layers) &
			to_string (circle),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		-- make sure the desired layers are available according to current layer stack:
		test_layers (module_cursor, circle.layers);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> draw'access);
		
	end draw_via_restrict_circle;

	
	procedure delete_via_restrict (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_via_restrict_lines;
			use pac_via_restrict_arcs;
			use pac_via_restrict_circles;
			line_cursor   : pac_via_restrict_lines.cursor  := module.board.via_restrict.lines.first;
			arc_cursor    : pac_via_restrict_arcs.cursor   := module.board.via_restrict.arcs.first;
			circle_cursor : pac_via_restrict_circles.cursor := module.board.via_restrict.circles.first;

			deleted : boolean := false; -- goes true if at least one segment has been deleted
		begin
			-- first search for a matching segment among the lines
			while line_cursor /= pac_via_restrict_lines.no_element loop
				if element (line_cursor).on_line (point) then
				-- CS use get_shortest_distance (point, element (line_cursor)
				-- and compare distance with accuracy	
					delete (module.board.via_restrict.lines, line_cursor);
					deleted := true;
					exit;
				end if;
				next (line_cursor);
			end loop;

			-- if no line found, search among arcs
			if not deleted then
				while arc_cursor /= pac_via_restrict_arcs.no_element loop
					
					if element (arc_cursor).on_arc (point) then
						-- CS use get_shortest_distance (point, element (arc_cursor)
						-- and compare distance with accuracy	

						delete (module.board.via_restrict.arcs, arc_cursor);
						deleted := true;
						exit;
					end if;
					
					next (arc_cursor);
				end loop;
			end if;

			-- if no arc found, search among circles
			if not deleted then
				while circle_cursor /= pac_via_restrict_circles.no_element loop
					
					if element (circle_cursor).on_circle (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						delete (module.board.via_restrict.circles, circle_cursor);
						deleted := true;
						exit;
					end if;
					
					next (circle_cursor);
				end loop;
			end if;

			if not deleted then
				no_segment_found (point, accuracy);
			end if;
			
		end delete;
		
	begin -- delete_via_restrict
		log (text => "module " & to_string (module_name) &
			" deleting via restrict segment" &
			" at" & to_string (point) &
			" accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_via_restrict;


	
-- BOARD OUTLINE / HOLES / CONTOUR / EDGE CUTS

	procedure set_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		outline			: in type_outer_contour;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is begin
			module.board.contours.outline := outline;
		end;
							   
	begin
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			 & " setting outline" & to_string (outline),
			level => log_threshold);

		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end set_outline;



	function get_outline (
		module_cursor	: in pac_generic_modules.cursor)
		return type_outer_contour
	is begin
		return element (module_cursor).board.contours.outline;
	end get_outline;


	
	function get_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		log_threshold	: in type_log_level)
		return type_outer_contour
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being queried
	begin
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			 & " getting outline",
			level => log_threshold);

		module_cursor := locate_module (module_name);
		
		return get_outline (module_cursor);
	end get_outline;



	procedure delete_outline (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is			
			deleted : boolean := false; -- goes true if at least one segment has been deleted

			procedure delete_segment is 
				use pac_segments;
				c : pac_segments.cursor;
			begin
				c := module.board.contours.outline.contour.segments.first;
				
				while c /= pac_segments.no_element loop

					case element (c).shape is
						when LINE =>
							if element (c).segment_line.on_line (point) then
								-- CS use get_shortest_distance (point, element)
								-- and compare distance with accuracy	

								delete (module.board.contours.outline.contour.segments, c);
								deleted := true;

								-- CS update start/end point of predecessor/successor segment
								
								exit; -- CS no exit if all segments are to be deleted
							end if;

						when ARC =>
							if element (c).segment_arc.on_arc (point) then
								-- CS use get_shortest_distance (point, element)
								-- and compare distance with accuracy	

								delete (module.board.contours.outline.contour.segments, c);
								deleted := true;

								-- CS update start/end point of predecessor/successor segment
								
								exit; -- CS no exit if all segments are to be deleted
							end if;

					end case;
					
					next (c);
				end loop;
			end delete_segment;

			procedure delete_circle is begin
				if module.board.contours.outline.contour.circle.on_circle (point) then
					-- CS use get_shortest_distance (point, element)
					-- and compare distance with accuracy	

					module.board.contours.outline.contour := (others => <>);					
					deleted := true;
				end if;
			end delete_circle;

			
		begin -- delete
			if module.board.contours.outline.contour.circular then
				delete_circle;				
			else
				delete_segment;
			end if;
			
			if not deleted then
				no_segment_found (point, accuracy);
			end if;			
		end delete;

		
	begin -- delete_outline
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			& " deleting outline segment at" & to_string (point) 
			& " accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_outline;

	

	procedure add_hole (
		module_cursor	: in pac_generic_modules.cursor;
		hole			: in type_hole;
		log_threshold	: in type_log_level)
	is

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is 
			use pac_holes;
		begin
			append (module.board.contours.holes, hole);
		end;
							   
	begin
		log (text => "module " & enclose_in_quotes (to_string (key (module_cursor))) 
			 & " adding hole" & to_string (hole),
			level => log_threshold);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end add_hole;

	
	
	procedure add_hole (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		hole			: in type_hole;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module)
		is 
			use pac_holes;
		begin
			append (module.board.contours.holes, hole);
		end;
							   
	begin
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			 & " adding hole" & to_string (hole),
			level => log_threshold);

		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end add_hole;


	function get_holes (
		module_cursor	: in pac_generic_modules.cursor)
		return pac_holes.list
	is begin
		return element (module_cursor).board.contours.holes;
	end get_holes;
	


	procedure delete_hole (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

	begin -- delete_hole
		log (text => "module " & enclose_in_quotes (to_string (module_name)) 
			& " deleting hole segment at" & to_string (point) 
			& " accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		module_cursor := locate_module (module_name);

		--update_element (
			--container	=> generic_modules,
			--position	=> module_cursor,
			--process		=> delete'access);

		-- CS
		
	end delete_hole;



	
-- SILK SCREEN

	procedure draw_silk_screen_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_silk_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is			
			use pac_silk_lines;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.silk_screen.top.lines,
						new_item	=> line);
					
				when BOTTOM =>
					append (
						container	=> module.board.silk_screen.bottom.lines,
						new_item	=> line);
			end case;
		end;
							   
	begin -- draw_silk_scree_line
		log (text => "module " & to_string (module_name) &
			" drawing silk screen line" &
			" face" & to_string (face) &
			to_string (line),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_silk_screen_line;

	
	procedure draw_silk_screen_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_silk_arc;		
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_silk_arcs;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.silk_screen.top.arcs,
						new_item	=> arc);

				when BOTTOM =>
					append (
						container	=> module.board.silk_screen.bottom.arcs,
						new_item	=> arc);
			end case;
		end;
							   
	begin -- draw_silk_screen_arc
		log (text => "module " & to_string (module_name) &
			" drawing silk screen arc" &
			" face" & to_string (face) &
			to_string (arc) &
			" width" & to_string (arc.width),

			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_silk_screen_arc;

	
	procedure draw_silk_screen_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_silk_circle;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_silk_circles;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.silk_screen.top.circles,
						new_item	=> circle);

				when BOTTOM =>
					append (
						container	=> module.board.silk_screen.bottom.circles,
						new_item	=> circle);

			end case;
		end;
							   
	begin -- draw_silk_screen_circle
		log (text => "module " & to_string (module_name) &
			" drawing silk screen circle" &
			" face" & to_string (face) &
			to_string (circle),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_silk_screen_circle;

	
	procedure delete_silk_screen (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_silk_lines;
			use pac_silk_arcs;
			use pac_silk_circles;
			line_cursor   : pac_silk_lines.cursor;
			arc_cursor    : pac_silk_arcs.cursor;
			circle_cursor : pac_silk_circles.cursor;

			deleted : boolean := false; -- goes true if at least one segment has been deleted
		begin
			if face = TOP then
				line_cursor   	:= module.board.silk_screen.top.lines.first;
				arc_cursor    	:= module.board.silk_screen.top.arcs.first;
				circle_cursor	:= module.board.silk_screen.top.circles.first;
			else
				line_cursor   	:= module.board.silk_screen.bottom.lines.first;
				arc_cursor    	:= module.board.silk_screen.bottom.arcs.first;
				circle_cursor	:= module.board.silk_screen.bottom.circles.first;
			end if;
			
			-- first search for a matching segment among the lines
			while line_cursor /= pac_silk_lines.no_element loop
				if element (line_cursor).on_line (point) then
				-- CS use get_shortest_distance (point, element)
				-- and compare distance with accuracy	

					if face = TOP then
						delete (module.board.silk_screen.top.lines, line_cursor);
					else
						delete (module.board.silk_screen.bottom.lines, line_cursor);
					end if;
					deleted := true;
					exit;
				end if;
				next (line_cursor);
			end loop;

			-- if no line found, search among arcs
			if not deleted then
				while arc_cursor /= pac_silk_arcs.no_element loop
					if element (arc_cursor).on_arc (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.silk_screen.top.arcs, arc_cursor);
						else
							delete (module.board.silk_screen.bottom.arcs, arc_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (arc_cursor);
				end loop;
			end if;

			-- if no arc found, search among circles
			if not deleted then
				while circle_cursor /= pac_silk_circles.no_element loop
					
					if element (circle_cursor).on_circle (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.silk_screen.top.circles, circle_cursor);
						else
							delete (module.board.silk_screen.bottom.circles, circle_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (circle_cursor);
				end loop;
			end if;

			if not deleted then
				no_segment_found (point, accuracy);
			end if;
			
		end delete;

		
	begin -- delete_silk_screen
		log (text => "module " & to_string (module_name) &
			" deleting silk screen segment face" & to_string (face) &
			" at" & to_string (point) &
			" accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_silk_screen;

	
	
-- ASSEMBLY DOCUMENTATION
	
	procedure draw_assy_doc_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in et_assy_doc.type_doc_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use et_assy_doc.pac_doc_lines;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.assy_doc.top.lines,
						new_item	=> line);
					
				when BOTTOM =>
					append (
						container	=> module.board.assy_doc.bottom.lines,
						new_item	=> line);
			end case;
		end;
							   
	begin -- draw_assy_doc_line
		log (text => "module " & to_string (module_name) &
			" drawing assembly documentation line" &
			" face" & to_string (face) &
			to_string (line),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_assy_doc_line;

	
	procedure draw_assy_doc_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_doc_arc;		
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_doc_arcs;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.assy_doc.top.arcs,
						new_item	=> arc);

				when BOTTOM =>
					append (
						container	=> module.board.assy_doc.bottom.arcs,
						new_item	=> arc);
			end case;
		end;
		
	begin -- draw_assy_doc_arc
		log (text => "module " & to_string (module_name) &
			" drawing assembly documentation arc" &
			" face" & to_string (face) &
			to_string (arc) &
			" width" & to_string (arc.width),

			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_assy_doc_arc;

	
	procedure draw_assy_doc_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_fillable_circle;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_doc_circles;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.assy_doc.top.circles,
						new_item	=> circle);

				when BOTTOM =>
					append (
						container	=> module.board.assy_doc.bottom.circles,
						new_item	=> circle);

			end case;
		end;
							   
	begin -- draw_assy_doc_circle
		log (text => "module " & to_string (module_name) &
			" drawing assembly documentation circle" &
			" face" & to_string (face) &
			to_string (circle),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_assy_doc_circle;

	
	procedure delete_assy_doc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_doc_lines;
			use pac_doc_arcs;
			use pac_doc_circles;
			line_cursor   : pac_doc_lines.cursor;
			arc_cursor    : pac_doc_arcs.cursor;
			circle_cursor : pac_doc_circles.cursor;

			deleted : boolean := false; -- goes true if at least one segment has been deleted
		begin
			if face = TOP then
				line_cursor   	:= module.board.assy_doc.top.lines.first;
				arc_cursor    	:= module.board.assy_doc.top.arcs.first;
				circle_cursor	:= module.board.assy_doc.top.circles.first;
			else
				line_cursor   	:= module.board.assy_doc.bottom.lines.first;
				arc_cursor    	:= module.board.assy_doc.bottom.arcs.first;
				circle_cursor	:= module.board.assy_doc.bottom.circles.first;
			end if;
			
			-- first search for a matching segment among the lines
			while line_cursor /= pac_doc_lines.no_element loop
				if element (line_cursor).on_line (point) then
					-- CS use get_shortest_distance (point, element)
					-- and compare distance with accuracy	

					if face = TOP then
						delete (module.board.assy_doc.top.lines, line_cursor);
					else
						delete (module.board.assy_doc.bottom.lines, line_cursor);
					end if;
					deleted := true;
					exit;
				end if;
				next (line_cursor);
			end loop;

			-- if no line found, search among arcs
			if not deleted then
				while arc_cursor /= pac_doc_arcs.no_element loop
					if element (arc_cursor).on_arc (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.assy_doc.top.arcs, arc_cursor);
						else
							delete (module.board.assy_doc.bottom.arcs, arc_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (arc_cursor);
				end loop;
			end if;

			-- if no arc found, search among circles
			if not deleted then
				while circle_cursor /= pac_doc_circles.no_element loop
					
					if element (circle_cursor).on_circle (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.assy_doc.top.circles, circle_cursor);
						else
							delete (module.board.assy_doc.bottom.circles, circle_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (circle_cursor);
				end loop;
			end if;

			if not deleted then
				no_segment_found (point, accuracy);
			end if;
			
		end delete;
		
	begin -- delete_assy_doc
		log (text => "module " & to_string (module_name) &
			" deleting assembly documentation segment face" & to_string (face) &
			" at" & to_string (point) &
			" accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_assy_doc;



	
-- STOP MASK
	
	procedure draw_stop_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_stop_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_stop_lines;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.stop_mask.top.lines,
						new_item	=> line);
					
				when BOTTOM =>
					append (
						container	=> module.board.stop_mask.bottom.lines,
						new_item	=> line);
			end case;
		end;
							   
	begin -- draw_stop_line
		log (text => "module " & to_string (module_name) &
			" drawing stop mask line" &
			" face" & to_string (face) &
			to_string (line),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_stop_line;

	
	procedure draw_stop_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_stop_arc;		
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_stop_arcs;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.stop_mask.top.arcs,
						new_item	=> arc);

				when BOTTOM =>
					append (
						container	=> module.board.stop_mask.bottom.arcs,
						new_item	=> arc);
			end case;
		end;
							   
	begin -- draw_stop_arc
		log (text => "module " & to_string (module_name) &
			" drawing stop mask arc" &
			" face" & to_string (face) &
			to_string (arc) &
			" width" & to_string (arc.width),

			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_stop_arc;
	

	procedure draw_stop_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_stop_circle;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_stop_circles;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.stop_mask.top.circles,
						new_item	=> circle);

				when BOTTOM =>
					append (
						container	=> module.board.stop_mask.bottom.circles,
						new_item	=> circle);

			end case;
		end;
							   
	begin -- draw_stop_circle
		log (text => "module " & to_string (module_name) &
			" drawing stop mask circle" &
			" face" & to_string (face) &
			to_string (circle),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_stop_circle;

	
	procedure delete_stop (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_stop_lines;
			use pac_stop_arcs;
			use pac_stop_circles;
			line_cursor   : pac_stop_lines.cursor;
			arc_cursor    : pac_stop_arcs.cursor;
			circle_cursor : pac_stop_circles.cursor;

			deleted : boolean := false; -- goes true if at least one segment has been deleted
		begin
			if face = TOP then
				line_cursor   	:= module.board.stop_mask.top.lines.first;
				arc_cursor    	:= module.board.stop_mask.top.arcs.first;
				circle_cursor	:= module.board.stop_mask.top.circles.first;
			else
				line_cursor   	:= module.board.stop_mask.bottom.lines.first;
				arc_cursor    	:= module.board.stop_mask.bottom.arcs.first;
				circle_cursor	:= module.board.stop_mask.bottom.circles.first;
			end if;
			
			-- first search for a matching segment among the lines
			while line_cursor /= pac_stop_lines.no_element loop
				if element (line_cursor).on_line (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
					if face = TOP then
						delete (module.board.stop_mask.top.lines, line_cursor);
					else
						delete (module.board.stop_mask.bottom.lines, line_cursor);
					end if;
					deleted := true;
					exit;
				end if;
				next (line_cursor);
			end loop;

			-- if no line found, search among arcs
			if not deleted then
				while arc_cursor /= pac_stop_arcs.no_element loop
					if element (arc_cursor).on_arc (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.stop_mask.top.arcs, arc_cursor);
						else
							delete (module.board.stop_mask.bottom.arcs, arc_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (arc_cursor);
				end loop;
			end if;

			-- if no arc found, search among circles
			if not deleted then
				while circle_cursor /= pac_stop_circles.no_element loop

					if element (circle_cursor).on_circle (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.stop_mask.top.circles, circle_cursor);
						else
							delete (module.board.stop_mask.bottom.circles, circle_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (circle_cursor);
				end loop;
			end if;

			if not deleted then
				no_segment_found (point, accuracy);
			end if;
			
		end delete;
		
	begin -- delete_stop
		log (text => "module " & to_string (module_name) &
			" deleting stop mask segment face" & to_string (face) &
			" at" & to_string (point) &
			" accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_stop;


	
-- STENCIL
	
	procedure draw_stencil_line (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		line			: in type_stencil_line;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_stencil_lines;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.stencil.top.lines,
						new_item	=> line);
					
				when BOTTOM =>
					append (
						container	=> module.board.stencil.bottom.lines,
						new_item	=> line);
			end case;
		end;
							   
	begin -- draw_stencil_line
		log (text => "module " & to_string (module_name) &
			" drawing stencil line" &
			" face" & to_string (face) &
			to_string (line),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_stencil_line;

	
	procedure draw_stencil_arc (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		arc				: in type_stencil_arc;		
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_stencil_arcs;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.stencil.top.arcs,
						new_item	=> arc);

				when BOTTOM =>
					append (
						container	=> module.board.stencil.bottom.arcs,
						new_item	=> arc);
			end case;
		end;
							   
	begin -- draw_stencil_arc
		log (text => "module " & to_string (module_name) &
			" drawing stencil arc" &
			" face" & to_string (face) &
			to_string (arc) &
			" width" & to_string (arc.width),

			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_stencil_arc;

	
	procedure draw_stencil_circle (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		circle			: in type_stencil_circle;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure add (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_stencil_circles;
		begin
			case face is
				when TOP =>
					append (
						container	=> module.board.stencil.top.circles,
						new_item	=> circle);

				when BOTTOM =>
					append (
						container	=> module.board.stencil.bottom.circles,
						new_item	=> circle);

			end case;
		end;

		
	begin -- draw_stencil_circle
		log (text => "module " & to_string (module_name) &
			" drawing stencil circle" &
			" face" & to_string (face) &
			to_string (circle),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> add'access);

	end draw_stencil_circle;
	

	procedure delete_stencil (
		module_name		: in pac_module_name.bounded_string; -- motor_driver (without extension *.mod)
		face			: in type_face;
		point			: in type_point; -- x/y
		accuracy		: in type_catch_zone;
		log_threshold	: in type_log_level) 
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		procedure delete (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_stencil_lines;
			use pac_stencil_arcs;
			use pac_stencil_circles;
			line_cursor   : pac_stencil_lines.cursor;
			arc_cursor    : pac_stencil_arcs.cursor;
			circle_cursor : pac_stencil_circles.cursor;

			deleted : boolean := false; -- goes true if at least one segment has been deleted
		begin
			if face = TOP then
				line_cursor   	:= module.board.stencil.top.lines.first;
				arc_cursor    	:= module.board.stencil.top.arcs.first;
				circle_cursor	:= module.board.stencil.top.circles.first;
			else
				line_cursor   	:= module.board.stencil.bottom.lines.first;
				arc_cursor    	:= module.board.stencil.bottom.arcs.first;
				circle_cursor	:= module.board.stencil.bottom.circles.first;
			end if;
			
			-- first search for a matching segment among the lines
			while line_cursor /= pac_stencil_lines.no_element loop
				if element (line_cursor).on_line (point) then
					-- CS use get_shortest_distance (point, element)
					-- and compare distance with accuracy	
					if face = TOP then
						delete (module.board.stencil.top.lines, line_cursor);
					else
						delete (module.board.stencil.bottom.lines, line_cursor);
					end if;
					deleted := true;
					exit;
				end if;
				next (line_cursor);
			end loop;

			-- if no line found, search among arcs
			if not deleted then
				while arc_cursor /= pac_stencil_arcs.no_element loop
					if element (arc_cursor).on_arc (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.stencil.top.arcs, arc_cursor);
						else
							delete (module.board.stencil.bottom.arcs, arc_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (arc_cursor);
				end loop;
			end if;

			-- if no arc found, search among circles
			if not deleted then
				while circle_cursor /= pac_stencil_circles.no_element loop
					
					if element (circle_cursor).on_circle (point) then
						-- CS use get_shortest_distance (point, element)
						-- and compare distance with accuracy	
						if face = TOP then
							delete (module.board.stencil.top.circles, circle_cursor);
						else
							delete (module.board.stencil.bottom.circles, circle_cursor);
						end if;
						deleted := true;
						exit;
					end if;
					next (circle_cursor);
				end loop;
			end if;

			if not deleted then
				no_segment_found (point, accuracy);
			end if;
			
		end delete;
		
	begin -- delete_stencil
		log (text => "module " & to_string (module_name) &
			" deleting stencil segment face" & to_string (face) &
			" at" & to_string (point) &
			" accuracy" & catch_zone_to_string (accuracy),
			level => log_threshold);

		-- locate module
		module_cursor := locate_module (module_name);

		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> delete'access);
		
	end delete_stencil;


	procedure place_text_in_non_conductor_layer (
		module_cursor	: in pac_generic_modules.cursor;
		layer_category	: in type_layer_category_non_conductor;
		face			: in type_face; -- top/bottom
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level)
	is 
		
		procedure place_text (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use et_text;
			use pac_contour_texts;
			use pac_assy_doc_texts;
			use pac_silk_texts;
			use pac_stop_texts;

			v_text : type_vector_text;		
			mirror : type_vector_text_mirrored;
		begin
			-- NOTE: Texts in bottom keepout are never mirrored:
			if face = BOTTOM and layer_category = LAYER_CAT_KEEPOUT then
				mirror := NO;
			else
				mirror := face_to_mirror (face);
			end if;
			
			v_text := vectorize_text (
				content		=> text.content,
				size		=> text.size,
				rotation	=> get_rotation (text.position),
				position	=> text.position.place,
				mirror		=> mirror,
				line_width	=> text.line_width
				-- CS alignment
				); 
			
			case layer_category is
				when LAYER_CAT_ASSY =>
					case face is
						when TOP =>
							append (module.board.assy_doc.top.texts, (text with v_text));
						when BOTTOM =>
							append (module.board.assy_doc.bottom.texts, (text with v_text));
					end case;

				when LAYER_CAT_SILKSCREEN =>
					case face is
						when TOP =>
							append (module.board.silk_screen.top.texts, (text with v_text));
						when BOTTOM =>
							append (module.board.silk_screen.bottom.texts, (text with v_text));
					end case;
					
				when LAYER_CAT_STOP =>
					case face is
						when TOP =>
							append (module.board.stop_mask.top.texts, (text with v_text));
						when BOTTOM =>
							append (module.board.stop_mask.bottom.texts, (text with v_text));
					end case;

				when others => null;
			end case;
		end place_text;

	begin -- place_text_in_non_conductor_layer
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " placing text in non-conductor layer at" -- CS output category
			& to_string (text.position)
			& " face" & to_string (face),
			level => log_threshold);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> place_text'access);

	end place_text_in_non_conductor_layer;

	
	procedure place_text_in_outline_layer (
		module_cursor	: in pac_generic_modules.cursor;
		layer_category	: in type_layer_category_outline;
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level)
	is 
		procedure place_text (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			v_text : type_vector_text;		
			use pac_contour_texts;
		begin
			v_text := vectorize_text (
				content		=> text.content,
				size		=> text.size,
				rotation	=> get_rotation (text.position),
				position	=> text.position.place,
				line_width	=> text.line_width
				-- CS alignment
				); 


			case layer_category is
				when LAYER_CAT_OUTLINE =>
					append (module.board.contours.texts, (text with v_text));

				when others => null; -- CS
			end case;
		end place_text;

	begin
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " placing text in outline layer at"
			& to_string (text.position),
			level => log_threshold);
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> place_text'access);

	end place_text_in_outline_layer;

	
	procedure place_text_in_conductor_layer (
		module_cursor	: in pac_generic_modules.cursor;
		signal_layer	: in type_signal_layer;
		text			: in type_text_fab_with_content;
		log_threshold	: in type_log_level)
	is
		procedure place_text (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_conductor_texts;
			vectors : pac_character_lines.list;
			use et_text;
			mirror : type_vector_text_mirrored;

			v_text : type_vector_text;
			c_text : type_conductor_text;
			
		begin
			mirror := signal_layer_to_mirror (signal_layer, deepest_conductor_layer (module_cursor));

			if mirror = YES then
				log (text => "text is in deepest signal layer -> will be mirrored", level => log_threshold + 1);
			else
				log (text => "text is not in deepest signal layer -> no mirroring", level => log_threshold + 1);
			end if;

			
			v_text := vectorize_text (
				content		=> text.content,
				size		=> text.size,
				rotation	=> get_rotation (text.position),
				position	=> text.position.place,
				mirror		=> mirror,
				line_width	=> text.line_width,
				make_border	=> true -- CS should be false for restrict layers
				-- CS alignment
				); 

			-- assemble the conductor text:
			c_text := (text with 
				layer		=> signal_layer,
				vectors		=> v_text -- CS call vectorize_text here directly
				--segments	=> make_segments (v_text, text.line_width)
				);
			
			append (module.board.conductors.texts, c_text);
		end place_text;

	begin
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " placing text in conductor layer at"
			& to_string (text.position)
			& " signal layer " & to_string (signal_layer),
			level => log_threshold);

		log_indentation_up;
		
		update_element (
			container	=> generic_modules,
			position	=> module_cursor,
			process		=> place_text'access);

		log_indentation_down;
	end place_text_in_conductor_layer;



	procedure place_fill_zone (
		module_cursor	: in pac_generic_modules.cursor;
		zone			: in type_zone'class;
		log_threshold	: in type_log_level;
		net_name		: in pac_net_name.bounded_string := no_name)
	is
		use ada.tags;
		use et_nets;
		
		
		procedure floating_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_floating_solid;

			p : type_floating_solid := 
				type_floating_solid (zone);
			
		begin
			log (text => to_string (p, p.properties),
				level => log_threshold + 1);

			module.board.conductors.fill_zones.solid.append (p);
		end floating_solid;

		
		procedure floating_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_floating_hatched;

			p : type_floating_hatched := 
				type_floating_hatched (zone);
			
		begin
			log (text => to_string (p, p.properties),
				level => log_threshold + 1);

			module.board.conductors.fill_zones.hatched.append (p);
		end floating_hatched;

		-- Polygons which are connected with a net are part of a route.
		-- They must be added to the targeted net. So we need a cursor
		-- to the targeted net:
		net_cursor : pac_nets.cursor;

		
		procedure locate_targeted_net is begin
			net_cursor := locate_net (module_cursor, net_name);

			if net_cursor = pac_nets.no_element then
				raise semantic_error_1 with
					"ERROR: Net " & enclose_in_quotes (to_string (net_name)) 
					& " does not exist !";
			end if;
		end locate_targeted_net;

		
		procedure route_solid (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_route_solid;

			p : type_route_solid := 
				type_route_solid (zone);


			procedure add_polygon (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				net.route.fill_zones.solid.append (p);
			end add_polygon;
			
		begin --route_solid
			log (text => to_string (p, p.properties, net_name),
				level => log_threshold + 1);

			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> add_polygon'access);
			
		end route_solid;

		
		procedure route_hatched (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use pac_route_hatched;

			p : type_route_hatched := 
				type_route_hatched (zone);
			
			procedure add_polygon (
				net_name	: in pac_net_name.bounded_string;
				net			: in out type_net)
			is begin
				net.route.fill_zones.hatched.append (p);
			end add_polygon;

		begin -- route_hatched
			log (text => to_string (p, p.properties, net_name),
				level => log_threshold + 1);

			update_element (
				container	=> module.nets,
				position	=> net_cursor,
				process		=> add_polygon'access);

		end route_hatched;

		
	begin -- place_fill_zone
		log (text => "module " 
			& enclose_in_quotes (to_string (key (module_cursor)))
			& " placing fill zone in conductor layer ...",
			level => log_threshold);

		log_indentation_up;
		
		-- floating:
		if zone'tag = type_floating_solid'tag then

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> floating_solid'access);

		elsif zone'tag = type_floating_hatched'tag then

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> floating_hatched'access);


		-- route:
		elsif zone'tag = type_route_solid'tag then

			locate_targeted_net;
						
			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> route_solid'access);

		elsif zone'tag = type_route_hatched'tag then

			locate_targeted_net;

			update_element (
				container	=> generic_modules,
				position	=> module_cursor,
				process		=> route_hatched'access);
			
		else
			null; -- CS ?
		end if;
		
		log_indentation_down;
	end place_fill_zone;



	procedure fill_fill_zones (
		module_cursor	: in pac_generic_modules.cursor;
		log_category	: in type_log_category;
		log_threshold	: in type_log_level;
		nets 			: in pac_net_names.list := no_net_names)
	is separate;
	
end et_board_ops;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
