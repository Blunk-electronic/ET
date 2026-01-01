------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             MODULE WRITE                                 --
--                                                                          --
-- Copyright (C) 2017 - 2025                                                --
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
-- ToDo:
-- - clean up
-- - simplify code
-- - use renames instead of element (c)
-- - separate schematic and board stuff in et_module_write.board/schematic
--

with ada.text_io;					use ada.text_io;
with ada.characters;				use ada.characters;
with ada.characters.latin_1;
with ada.characters.handling;		use ada.characters.handling;
with ada.strings;					use ada.strings;
with ada.strings.fixed; 			use ada.strings.fixed;
with ada.directories;				use ada.directories;
with ada.exceptions;
with gnat.directory_operations;
with et_directory_and_file_ops;

with et_text_content;				use et_text_content;
with et_general_rw;					use et_general_rw;
with et_system_info;
with et_string_processing;			use et_string_processing;
with et_schematic_geometry;
with et_schematic_coordinates;

with et_section_headers;			use et_section_headers;
with et_keywords;					use et_keywords;
with et_module_sections;			use et_module_sections;
with et_pcb_sides;
with et_board_geometry;
with et_board_coordinates;

with et_assembly_variants;			use et_assembly_variants;
with et_assembly_variant_name;		use et_assembly_variant_name;
with et_coordinates_formatting;		use et_coordinates_formatting;

with et_design_rules;				use et_design_rules;
with et_design_rules_board;			use et_design_rules_board;

with et_net_names;
with et_net_class;
with et_net_class_name;
with et_net_class_description;
with et_net_classes;
with et_device_name;				use et_device_name;
with et_device_model;
with et_module_instance;
with et_device_purpose;
with et_device_model_names;
with et_device_value;
with et_device_partcode;
with et_schematic_text;
with et_sheets;
with et_pcb;
with et_pcb_stack;
with et_pcb_signal_layers;			use et_pcb_signal_layers;

with et_board_write;				use et_board_write;

with et_time;						use et_time;

with et_schematic_ops;

with et_board_ops;

with et_schematic_text;
with et_board_text;

with et_board_layer_category;			use et_board_layer_category;
with et_submodules;
with et_netlists;
with et_alignment;						use et_alignment;

with et_module_write_meta;				use et_module_write_meta;
with et_module_write_grid;				use et_module_write_grid;
with et_module_write_frames;			use et_module_write_frames;
with et_module_write_board_outline;		use et_module_write_board_outline;
with et_module_write_freetracks;		use et_module_write_freetracks;
with et_module_write_board_zones;		use et_module_write_board_zones;
with et_module_write_text_board;		use et_module_write_text_board;
with et_module_write_silkscreen;		use et_module_write_silkscreen;
with et_module_write_assy_doc;			use et_module_write_assy_doc;
with et_module_write_stopmask;			use et_module_write_stopmask;
with et_module_write_stencil;			use et_module_write_stencil;
with et_module_write_route_restrict;	use et_module_write_route_restrict;
with et_module_write_nets;				use et_module_write_nets;

with et_module_write_devices_electrical;	use et_module_write_devices_electrical;
with et_module_write_device_non_electrical;	use et_module_write_device_non_electrical;



package body et_module_write is

	

	procedure write_module (
		module_cursor	: in pac_generic_modules.cursor;
		save_as_name	: in pac_module_name.bounded_string := to_module_name (""); -- motor_driver_test, templates/clock_generator_test
		log_threshold	: in type_log_level)
	is
		use pac_generic_modules;

		-- backup the previous output destination
		previous_output : ada.text_io.file_type renames current_output;
		
		module_file_handle : ada.text_io.file_type;


		-- Composes the target file name like bood-sample-tester.mod,
		-- creates the file (in the current working directory) and 
		-- directs all outputs to it.
		-- Writes s a nice header in the target file:
		procedure write_header is 
			use ada.directories;
			use gnat.directory_operations;
			-- use pac_project_name;
			-- use pac_project_path;
			use et_system_info;
		begin
			if pac_module_name.length (save_as_name) = 0 then
				-- The module is to be saved with its own name:

				log (text => "Saving module as " 
					& enclose_in_quotes (to_string (key (module_cursor))) & " ...",
					level => log_threshold + 1);
					
				-- Compose the target full file name and create the module file:
				create (
					file => module_file_handle,
					mode => out_file, 
					name => to_string (key (module_cursor))
							& latin_1.full_stop
							& module_file_name_extension
					);
			else
				-- The module is to be saved with a different name:

				log (text => "Saving module as " 
					& enclose_in_quotes (to_string (save_as_name)) & " ...",
					level => log_threshold + 1);

				create (
					file => module_file_handle,
					mode => out_file, 
					name => to_string (save_as_name)
							& latin_1.full_stop
							& module_file_name_extension
					);

			end if;
			
			-- write in a nice header
			set_output (module_file_handle);
			put_line (comment_mark & " " & system_name & " module");
			put_line (comment_mark & " " & get_date);
			put_line (comment_mark & " " & row_separator_double);
			new_line;
		end write_header;		


		-- Writes a nice footer in the target file and closes it.
		-- Directs subsequent outputs to the previous output (That
		-- is the output which was set before write_module has been called.):
		procedure write_footer is begin
			new_line;		
			log (text => "closing module file ...", level => log_threshold + 1);
			
			put_line (comment_mark & " " & row_separator_double);
			put_line (comment_mark & " " & get_date);
			put_line (comment_mark & " module file end");
			new_line;

			set_output (previous_output);
			close (module_file_handle);
		end write_footer;



		
		




		
		
		procedure query_rules is
			use et_board_ops;
			rules : constant type_design_rules := element (module_cursor).rules;
		begin
			log_indentation_up;
			log (text => "rules ...", level => log_threshold + 1);
			
			section_mark (section_rules, HEADER);

			-- Write the layout design rules. If none assigned to the
			-- module, write nothing:
			if layout_rules_assigned (module_cursor) then
				write (keyword => keyword_layout, parameters => to_string (rules.layout));
			end if;
																			
			section_mark (section_rules, FOOTER);

			log_indentation_down;
		end query_rules;



		
		
		procedure query_net_classes is
			use et_net_class;
			use et_net_class_name;
			use et_net_classes;
			use pac_net_classes;
			use et_net_class_description;
			use et_board_geometry.pac_geometry_2;

			
			procedure write (class_cursor : in pac_net_classes.cursor) is 
				name : string renames get_net_class_name (class_cursor);
				net_class : type_net_class renames element (class_cursor);
			begin
				log (text => "net class " & name, level => log_threshold + 1);
				section_mark (section_net_class, HEADER);

				write (keyword => keyword_name, parameters => name);
				write (keyword => keyword_description, parameters => to_string (net_class.description), wrap => true);
				write (keyword => keyword_clearance, parameters => to_string (net_class.clearance));
				write (keyword => keyword_track_width_min, parameters => to_string (net_class.track_width_min));
				write (keyword => keyword_via_drill_min, parameters => to_string (net_class.via_drill_min));
				write (keyword => keyword_via_restring_min, parameters => to_string (net_class.via_restring_min));
				write (keyword => keyword_micro_via_drill_min, parameters => to_string (net_class.micro_via_drill_min));
				write (keyword => keyword_micro_via_restring_min, parameters => to_string (net_class.micro_via_restring_min));

				section_mark (section_net_class, FOOTER);
			end write;

			
		begin
			log_indentation_up;
			
			section_mark (section_net_classes, HEADER);
			iterate (element (module_cursor).net_classes, write'access);
			section_mark (section_net_classes, FOOTER);

			log_indentation_down;
		end query_net_classes;




		
		
		procedure query_layer_stack is
			use et_board_geometry.pac_geometry_2;
			use et_pcb_stack;
			use package_layers;

			
			procedure query_layers (cursor : in package_layers.cursor) is
				layer : type_layer := element (cursor);
			begin
				-- write: "conductor   1 0.035"
				write (keyword => keyword_conductor,
					   parameters => 2 * space & to_string (to_index (cursor)) 
					   & to_string (layer.conductor.thickness));

				-- write "dielectric  1 0.200"
				write (keyword => keyword_dielectric, 
					   parameters => space & to_string (to_index (cursor)) 
					   & to_string (layer.dielectric.thickness));

			end;

			bottom_layer : type_signal_layer;
			bottom_layer_thickness : type_conductor_thickness;

			
		begin -- query_layer_stack
			log_indentation_up;
			
			section_mark (section_board_layer_stack, HEADER);

			-- iterate layers starting at top layer (1) until the deepest inner layer.
			-- The bottom layer is not part of the layer list and will be written later.
			iterate (element (module_cursor).board.stack.layers, query_layers'access);

			-- The bottom layer number is the deepest inner layer plus one:
			bottom_layer := last_index (element (module_cursor).board.stack.layers) + 1;

			-- Get the bottom conductor thickness:
			bottom_layer_thickness := element (module_cursor).board.stack.bottom.thickness;

			-- Write the bottom layer in the file.
			write (keyword => keyword_conductor,
				parameters => space & to_string (bottom_layer) & to_string (bottom_layer_thickness) &
				space & comment_mark & " bottom signal layer");
			
			section_mark (section_board_layer_stack, FOOTER);
			
			log_indentation_down;
			
		end query_layer_stack;

	



		
		-- writes the assembly variants in the module file
		procedure query_assembly_variants is
			use pac_assembly_variants;
			use et_device_value;
			use et_device_partcode;

			
			procedure query_devices (
				variant_name	: in pac_assembly_variant_name.bounded_string;
				variant			: in type_assembly_variant) 
			is
				use pac_device_variants;
				device_cursor : pac_device_variants.cursor := variant.devices.first;

				
				function purpose return string is 
					use et_device_purpose;
				begin
					if get_length (element (device_cursor).purpose) > 0 then
						return space & keyword_purpose & space &
							enclose_in_quotes (
								text_in => to_string (element (device_cursor).purpose),
								quote	=> latin_1.quotation);
					else
						return "";
					end if;
				end;

				use et_device_model;

				
			begin -- query_devices
				while device_cursor /= pac_device_variants.no_element loop
					case element (device_cursor).mounted is
						when NO =>
							write (
								keyword		=> keyword_device,
								parameters	=> to_string (key (device_cursor)) & 
												space & keyword_not_mounted);

						when YES =>
							write (
								keyword		=> keyword_device,
								parameters	=> to_string (key (device_cursor)) & 
									space &
									keyword_value & space &
									to_string (element (device_cursor).value) &
									space & keyword_partcode & space &
									to_string (element (device_cursor).partcode) &
									purpose);

					end case;
					
					next (device_cursor);
				end loop;
			end query_devices;


			
			procedure query_submodules (
				variant_name	: in pac_assembly_variant_name.bounded_string;
				variant			: in type_assembly_variant) 
			is
				use et_module_instance;
				use pac_submodule_variants;
				submodule_cursor : pac_submodule_variants.cursor := variant.submodules.first;
			begin
				while submodule_cursor /= pac_submodule_variants.no_element loop
					write (
						keyword		=> keyword_submodule,
						parameters	=> to_string (key (submodule_cursor)) &
										space & keyword_variant & space &
										to_variant (element (submodule_cursor).variant));
					
					next (submodule_cursor);
				end loop;
			end query_submodules;


			
			procedure write (variant_cursor : in pac_assembly_variants.cursor) is 
			begin
				section_mark (section_assembly_variant, HEADER);
				write (keyword => keyword_name, parameters => to_variant (key (variant_cursor)));
				write (keyword => keyword_description, wrap => true, parameters => to_string (element (variant_cursor).description));

				-- write the device variants
				query_element (
					position	=> variant_cursor,
					process		=> query_devices'access);

				-- write the submodule variants
				query_element (
					position	=> variant_cursor,
					process		=> query_submodules'access);
				
				section_mark (section_assembly_variant, FOOTER);
				new_line;
			end write;

			
		begin -- query_assembly_variants
			section_mark (section_assembly_variants, HEADER);

			-- Write assembly variants if variants exists for the module.
			-- If no variants exist, then this section will be left empty.
			if not is_empty (element (module_cursor).variants) then

				-- iterate assembly variants
				iterate (element (module_cursor).variants, write'access);

				-- write the active assembly variant
				write (
					keyword		=> keyword_active,
					parameters	=> to_variant (element (module_cursor).active_variant));

			end if;
			
			section_mark (section_assembly_variants, FOOTER);
		end query_assembly_variants;



		
		-- writes the netchangers in the module file
		procedure query_netchangers is
			use et_schematic_geometry;
			use et_schematic_coordinates;	
			use et_submodules;
			use pac_netchangers;

			
			procedure query_netchanger (cursor : pac_netchangers.cursor) is
				use pac_geometry_2;
			begin
				section_mark (section_netchanger, HEADER);
				write (keyword => keyword_name,	parameters => to_string (key (cursor))); -- 1, 2, 201, ...
				write (keyword => keyword_position_in_schematic, 
					parameters => to_string (element (cursor).position_sch, FORMAT_2)); -- position_in_schematic sheet 1 x 147.32 y 96.97

				write (
					keyword => keyword_rotation_in_schematic, 
					parameters => to_string (get_rotation (element (cursor).position_sch))); -- rotation_in_schematic 90.0

				write (
					keyword => keyword_position_in_board, 
					parameters => et_board_geometry.pac_geometry_2.to_string (element (cursor).position_brd)); -- position_in_board x 1.32 y 6.97
				
				write (keyword => keyword_layer, parameters => to_string (element (cursor).layer)); -- layer 2
				section_mark (section_netchanger, FOOTER);
			end query_netchanger;

			
		begin
			section_mark (section_netchangers, HEADER);
			iterate (element (module_cursor).netchangers, query_netchanger'access);
			section_mark (section_netchangers, FOOTER);
		end query_netchangers;



		

		
		procedure query_submodules is		
			use et_schematic_geometry.pac_geometry_2;
			use et_submodules;
			use pac_submodules;
			use et_net_names;
			use pac_net_name;

			
			procedure query_ports (port_cursor : in et_submodules.pac_submodule_ports.cursor) is
				use et_submodules.pac_submodule_ports;
			begin
				section_mark (section_port, HEADER);
				write (keyword => keyword_name, parameters => to_string (key (port_cursor))); -- name clk_out

				write (keyword => keyword_position, 
					parameters => to_string (element (port_cursor).position, FORMAT_2)); -- position x 0 y 10
				
				write (keyword => keyword_direction, parameters => to_string (element (port_cursor).direction)); -- direction master/slave
				section_mark (section_port, FOOTER);
			end;

			
			procedure write (submodule_cursor : in pac_submodules.cursor) is 
				use et_schematic_coordinates;
				use et_schematic_geometry.pac_geometry_2;
				use et_module_instance;
			begin
				section_mark (section_submodule, HEADER);
				write (keyword => keyword_name, parameters => to_string (key (submodule_cursor))); -- name stepper_driver_1
				write (keyword => keyword_file, parameters => pac_submodule_path.to_string (element (submodule_cursor).file)); -- file $ET_TEMPLATES/motor_driver.mod

				write (keyword => keyword_position, 
					parameters => to_string (element (submodule_cursor).position, FORMAT_2));
				
				write (keyword => keyword_size, parameters => 
					space & keyword_x & to_string (element (submodule_cursor).size.x) &
					space & keyword_y & to_string (element (submodule_cursor).size.y)); -- size x 50 y 70
				
				write (keyword => keyword_position_in_board, parameters => -- position_in_board x 23 y 0.2 rotation 90.0
					et_board_geometry.pac_geometry_2.to_string (element (submodule_cursor).position_in_board));

				write (keyword => keyword_view_mode, parameters => to_string (element (submodule_cursor).view_mode));

				section_mark (section_ports, HEADER);
				et_submodules.pac_submodule_ports.iterate (element (submodule_cursor).ports, query_ports'access);
				section_mark (section_ports, FOOTER);
				
				section_mark (section_submodule, FOOTER);				
			end write;

			
		begin -- query_submodules
			section_mark (section_submodules, HEADER);
			iterate (element (module_cursor).submods, write'access);
			section_mark (section_submodules, FOOTER);
		end query_submodules;



		
		procedure query_texts is	
			use et_schematic_text;
			use pac_text_schematic;
			use et_schematic_coordinates;
			use et_schematic_geometry.pac_geometry_2;
			use et_schematic_text;
			use pac_texts;
			
			
			procedure write (text_cursor : in pac_texts.cursor) is 
				use et_sheets;
			begin
				section_mark (section_text, HEADER);
				write
					(
					keyword		=> keyword_position,
					parameters	=> keyword_sheet & to_string (element (text_cursor).sheet) 
									& space & to_string (element (text_cursor).position, FORMAT_2)
					); -- position sheet 1 x 30 y 180
				
				write (keyword => keyword_rotation, 
						parameters => to_string (to_rotation (element (text_cursor).rotation)));
				
				write (keyword => keyword_content, wrap => true,
						parameters => to_string (element (text_cursor).content));
				
				write (keyword => keyword_size, parameters => to_string (element (text_cursor).size));
				write (keyword => keyword_alignment, parameters =>
					keyword_horizontal & space & to_string (element (text_cursor).alignment.horizontal)
					& space & keyword_vertical & space
					& to_string (element (text_cursor).alignment.vertical));

				-- CS font
				
				section_mark (section_text, FOOTER);
			end write;

			
		begin
			section_mark (section_texts, HEADER);
			iterate (element (module_cursor).texts, write'access);
			section_mark (section_texts, FOOTER);
		end query_texts;

		


		
		procedure query_board is
			use et_board_geometry.pac_contours;
			use et_pcb;
			use et_pcb_stack;
			use et_board_geometry.pac_geometry_2;

			
		
			procedure query_user_settings is
				use et_board_ops;
				us : constant et_pcb.type_user_settings := get_user_settings (module_cursor);

				
				procedure vias is begin
					section_mark (section_vias, HEADER);

					-- via drill
					if us.vias.drill.active then
						write ( -- drill 0.3
							keyword		=> keyword_via_drill, 
							parameters	=> to_string (us.vias.drill.size));
					else
						write ( -- drill dru
							keyword		=> keyword_via_drill, 
							parameters	=> keyword_dru);
					end if;

					-- inner restring
					if us.vias.restring_inner.active then
						write ( -- restring_inner 0.3
							keyword		=> keyword_restring_inner, 
							parameters	=> to_string (us.vias.restring_inner.width));
					else
						write ( -- restring_inner dru
							keyword		=> keyword_restring_inner, 
							parameters	=> keyword_dru);
					end if;

					-- outer restring
					if us.vias.restring_outer.active then
						write ( -- restring_outer 0.3
							keyword		=> keyword_restring_outer, 
							parameters	=> to_string (us.vias.restring_outer.width));
					else
						write ( -- restring_inner dru
							keyword		=> keyword_restring_outer, 
							parameters	=> keyword_dru);
					end if;
					
					section_mark (section_vias, FOOTER);
				end vias;

				
				procedure polygons is begin
					section_mark (section_fill_zones_conductor, HEADER);

					write_fill_style (us.polygons_conductor.fill_style);
					write_fill_linewidth (us.polygons_conductor.linewidth);
					write_priority (us.polygons_conductor.priority_level);
					write_isolation (us.polygons_conductor.isolation);
					
					write_spacing (us.polygons_conductor.spacing);
					
					write_pad_connection (us.polygons_conductor.connection);
					write_thermal (us.polygons_conductor.thermal);

					write_easing (us.polygons_conductor.easing);
					
					section_mark (section_fill_zones_conductor, FOOTER);
				end polygons;

				
			begin -- query_user_settings
				section_mark (section_user_settings, HEADER);

				vias;
				polygons;
					
				section_mark (section_user_settings, FOOTER);
			end query_user_settings;
			



			procedure write_silkscreen is
				use et_pcb_sides;
			begin
				section_mark (section_silkscreen, HEADER);

				section_mark (section_top, HEADER);
				
				-- lines, arcs, circles:
				write_silkscreen (module_cursor, TOP, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, TOP, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, TOP, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, TOP, log_threshold + 2);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);

				-- lines, arcs, circles:
				write_silkscreen (module_cursor, BOTTOM, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, BOTTOM, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, BOTTOM, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_SILKSCREEN, BOTTOM, log_threshold + 2);

				section_mark (section_bottom, FOOTER);
				
				section_mark (section_silkscreen, FOOTER);
			end write_silkscreen;



			
			procedure write_assy_doc is
				use et_pcb_sides;
			begin
				section_mark (section_assembly_doc, HEADER);

				section_mark (section_top, HEADER);

				-- lines, arcs, circles:
				write_assy_doc (module_cursor, TOP, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_ASSY, TOP, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_ASSY, TOP, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_ASSY, TOP, log_threshold + 2);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);

				-- lines, arcs, circles:
				write_assy_doc (module_cursor, BOTTOM, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_ASSY, BOTTOM, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_ASSY, BOTTOM, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_ASSY, BOTTOM, log_threshold + 2);
				section_mark (section_bottom, FOOTER);

				section_mark (section_assembly_doc, FOOTER);
			end write_assy_doc;
			

			

			procedure write_stencil is
				use et_pcb_sides;
			begin			
				section_mark (section_stencil, HEADER);

				section_mark (section_top, HEADER);
				write_stencil (module_cursor, TOP, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_STENCIL, TOP, log_threshold + 2);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				write_stencil (module_cursor, BOTTOM, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_STENCIL, BOTTOM, log_threshold + 2);
				section_mark (section_bottom, FOOTER);

				section_mark (section_stencil, FOOTER);
			end write_stencil;


			

			procedure write_stop_mask is
				use et_pcb_sides;
			begin
				section_mark (section_stopmask, HEADER);

				section_mark (section_top, HEADER);

				-- lines, arcs, circles:
				write_stopmask (module_cursor, TOP, log_threshold + 2);
				write_zones_non_conductor (module_cursor, LAYER_CAT_STOPMASK, TOP, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_STOPMASK, TOP, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_STOPMASK, TOP, log_threshold + 2);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);

				-- lines, arcs, circles:
				write_stopmask (module_cursor, BOTTOM, log_threshold + 2);				
				write_zones_non_conductor (module_cursor, LAYER_CAT_STOPMASK, BOTTOM, log_threshold + 2);
				write_texts_non_conductor (module_cursor, LAYER_CAT_STOPMASK, BOTTOM, log_threshold + 2);
				write_placeholders_non_conductor (module_cursor, LAYER_CAT_STOPMASK, BOTTOM, log_threshold + 2);
				section_mark (section_bottom, FOOTER);

				section_mark (section_stopmask, FOOTER);
			end write_stop_mask;




			procedure write_keepout is
				use et_pcb_sides;
			begin
				section_mark (section_keepout, HEADER);

				section_mark (section_top, HEADER);
				write_zones_non_conductor (module_cursor, LAYER_CAT_KEEPOUT, TOP, log_threshold + 2);
				write_zones_non_conductor_cutout (module_cursor, LAYER_CAT_KEEPOUT, TOP, log_threshold + 2);
				section_mark (section_top, FOOTER);

				section_mark (section_bottom, HEADER);
				write_zones_non_conductor (module_cursor, LAYER_CAT_KEEPOUT, BOTTOM, log_threshold + 2);
				write_zones_non_conductor_cutout (module_cursor, LAYER_CAT_KEEPOUT, BOTTOM, log_threshold + 2);
				section_mark (section_bottom, FOOTER);

				section_mark (section_keepout, FOOTER);
			end write_keepout;

			

			procedure write_route_restrict is begin
				section_mark (section_route_restrict, HEADER);

				-- lines, arcs, circles:
				write_route_restrict (module_cursor, log_threshold + 2);
				write_zones_route_restrict (module_cursor, log_threshold + 2);
				write_zones_route_restrict_cutout (module_cursor, log_threshold + 2);

				section_mark (section_route_restrict, FOOTER);
			end write_route_restrict;



			procedure write_via_restrict is begin
				section_mark (section_via_restrict, HEADER);
				write_zones_via_restrict (module_cursor, log_threshold + 2);
				write_zones_via_restrict_cutout (module_cursor, log_threshold + 2);
				section_mark (section_via_restrict, FOOTER);
			end write_via_restrict;



			procedure write_conductors_floating is begin
				section_mark (section_conductor, HEADER);

				write_freetracks (module_cursor, log_threshold + 2);
				write_zones_conductor (module_cursor, log_threshold + 2);
				write_zones_conductor_cutout (module_cursor, log_threshold + 2);
				write_texts_conductor (module_cursor, log_threshold + 2);
				write_placeholders_conductor (module_cursor, log_threshold + 2);

				section_mark (section_conductor, FOOTER);
			end;

			
		begin -- query_board
			section_mark (section_board, HEADER);

			-- USER SETTINGS
			query_user_settings;
		
			write_devices_non_electrical (module_cursor, log_threshold + 1);

			write_silkscreen;
			write_assy_doc;
			write_stencil;
			write_stop_mask;
			write_keepout;
			
			write_route_restrict;
			write_via_restrict;

			write_conductors_floating; -- NON-ELECTRIC !

			-- outer board contour and holes:
			write_board_outline (module_cursor, log_threshold + 1);
			
			---BOARD END-----
			section_mark (section_board, FOOTER);
		end query_board;

		
	begin -- write_module

		-- CS check if module is inside project directory ?
		
		write_header;

		-- meta data
		write_meta (module_cursor, log_threshold);
		put_line (row_separator_single);

		-- rules
		query_rules;
		put_line (row_separator_single);
		
		-- net classes
		query_net_classes;
		put_line (row_separator_single);

		-- drawing grid
		write_drawing_grid (module_cursor, log_threshold);
		put_line (row_separator_single);

		-- layer stack
		query_layer_stack;
		put_line (row_separator_single);
		
		-- nets
		write_nets (module_cursor, log_threshold);
		put_line (row_separator_single);
		
		-- frames
		write_frames (module_cursor, log_threshold);
		put_line (row_separator_single);
		
		-- notes
		query_texts;
		put_line (row_separator_single);
		
		-- submodules
		query_submodules;
		put_line (row_separator_single);
		
		-- devices
		write_devices_electrical (module_cursor, log_threshold);
		put_line (row_separator_single);

		-- assembly variants
		query_assembly_variants;
		put_line (row_separator_single);
		
		-- netchangers
		query_netchangers;
		put_line (row_separator_single);
		
		-- board
		query_board;
		put_line (row_separator_single);	

		write_footer;

		exception when event:
			others => 
				log (text => ada.exceptions.exception_message (event), console => true);
				close (module_file_handle);
				set_output (previous_output);
				raise;
		
	end write_module;


end et_module_write;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
