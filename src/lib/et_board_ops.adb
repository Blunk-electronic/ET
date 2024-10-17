------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                           BOARD OPERATIONS                               --
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

with et_assembly_variants;
with et_meta;
with et_netlists;
with et_device_query_schematic;		use et_device_query_schematic;
with et_schematic_ops.nets;			use et_schematic_ops.nets;
with et_schematic_ops.submodules;
with et_schematic_ops;				use et_schematic_ops;

with et_submodules;
with et_numbering;
with et_symbols;
with et_conductor_segment.boards;
with et_exceptions;					use et_exceptions;

with et_canvas_board_2;


package body et_board_ops is


	use pac_devices_sch;
	use pac_devices_non_electric;
	use pac_nets;


	
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
		point			: in type_vector_model; -- x/y
		log_threshold	: in type_log_level)
	is
		module_cursor : pac_generic_modules.cursor; -- points to the module being modified

		
		procedure query_submodules (
			module_name	: in pac_module_name.bounded_string;
			module		: in out type_module) 
		is
			use et_submodules.pac_submodules;
			submod_cursor : et_submodules.pac_submodules.cursor;

			use et_schematic_ops.submodules;

			
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

		-- CS update_ratsnest (module_cursor, log_threshold + 1);
		-- requires to move this procedure to a child package
		-- for operations on submodules.
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
						et_pcb_coordinates_2.type_package_position 
					is 
						-- Get the device position in the generic submodule:.
						device_position : et_pcb_coordinates_2.type_package_position := position_generic;
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

				use et_schematic_ops.submodules;
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
		et_schematic_ops.submodules.build_submodules_tree (
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


	
	
	procedure test_layers (
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




end et_board_ops;
	
-- Soli Deo Gloria


-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
