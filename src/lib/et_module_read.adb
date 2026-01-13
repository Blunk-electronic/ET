------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             MODULE READ                                  --
--                                                                          --
--                               B o d y                                    --
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
--                                                                          --
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
--
--

with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;
with ada.directories;				use ada.directories;

with et_directory_and_file_ops;

with et_module_names;				use et_module_names;
with et_section_headers;			use et_section_headers;
with et_keywords;					use et_keywords;
with et_module_sections;			use et_module_sections;
with et_pcb_sides;

with et_device_sections;
with et_package_sections;

with et_time;

with et_module;							use et_module;
with et_generic_modules;				use et_generic_modules;
-- with et_module_board;				use et_module_board;

with et_board_ops.ratsnest;

with et_board_layer_category;
with et_submodules;

with et_object_status;
with et_string_processing;					use et_string_processing;
with et_general_rw;							use et_general_rw;
with et_package_sections;					use et_package_sections;
with et_exceptions;							use et_exceptions;

with et_module_read_device_electrical;		use et_module_read_device_electrical;
with et_module_read_device_non_electrical;	use et_module_read_device_non_electrical;

with et_module_read_meta;					use et_module_read_meta;
with et_module_read_assembly_variant;		use et_module_read_assembly_variant;

with et_module_read_design_rules;			use et_module_read_design_rules;
with et_module_read_grid;					use et_module_read_grid;
with et_module_read_net_classes;			use et_module_read_net_classes;
with et_module_read_nets;					use et_module_read_nets;
with et_module_read_frames;					use et_module_read_frames;
with et_module_read_submodules;				use et_module_read_submodules;
with et_module_read_netchangers;			use et_module_read_netchangers;
with et_module_read_text_board;				use et_module_read_text_board;
with et_module_read_text_schematic;			use et_module_read_text_schematic;
with et_module_read_via;					use et_module_read_via;
with et_module_read_tracks_route;			use et_module_read_tracks_route;
with et_module_read_board_zones;			use et_module_read_board_zones;
with et_module_read_board_zones_route;		use et_module_read_board_zones_route;
with et_module_read_pcb_layer_stack;		use et_module_read_pcb_layer_stack;
with et_module_read_freetracks;				use et_module_read_freetracks;
with et_module_read_route_restrict;			use et_module_read_route_restrict;
with et_module_read_board_user_settings;	use et_module_read_board_user_settings;
with et_module_read_assy_doc;				use et_module_read_assy_doc;
with et_module_read_silkscreen;				use et_module_read_silkscreen;
with et_module_read_stopmask;				use et_module_read_stopmask;
with et_module_read_stencil;				use et_module_read_stencil;
with et_module_read_board_contour;			use et_module_read_board_contour;
with et_module_read_board_outline;			use et_module_read_board_outline;


package body et_module_read is

	use pac_generic_modules;

	
	procedure read_module (
		file_name 		: in string; -- motor_driver.mod, templates/clock_generator.mod
		log_threshold	: in type_log_level) 
	is
		
		previous_input : ada.text_io.file_type renames current_input;
		
		-- Environment variables like $templates could be in file name.
		-- In order to test whether the given module file exists, file name_name must be expanded
		-- so that the environment variables are replaced by the real paths like:
		-- templates/clock_generator.mod or
		-- /home/user/et_templates/pwr_supply.mod.
		use et_directory_and_file_ops;
		file_name_expanded : constant string := expand (file_name);
			
		file_handle : ada.text_io.file_type;
		use pac_generic_modules;
		module_cursor : pac_generic_modules.cursor; -- points to the module being read
		module_inserted : boolean;

		-- The line read from the the module file:
		line : et_string_processing.type_fields_of_line;

		-- This is the section stack of the module. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 11;
		package stack is new stack_lifo (
			item	=> type_module_section,
			max 	=> max_section_depth);


		
				

		
		
		procedure process_line is 
			
			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
			-- and finally assembled to actual objects:
			

				use et_board_layer_category;
				

				procedure insert_line (
					layer_cat	: in type_layer_category;
					face		: in et_pcb_sides.type_face) -- TOP, BOTTOM
				is begin
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							insert_silk_line (module_cursor, face, log_threshold);
							
						when LAYER_CAT_ASSY =>
							insert_doc_line (module_cursor, face, log_threshold);

						when LAYER_CAT_STENCIL =>
							insert_stencil_line (module_cursor, face, log_threshold);
							
						when LAYER_CAT_STOPMASK =>
							insert_stop_line (module_cursor, face, log_threshold);

						when others => null; -- CS raise exception ?								
					end case;
				end insert_line;


				
				
				procedure insert_arc (
					layer_cat	: in type_layer_category;
					face		: in et_pcb_sides.type_face) -- TOP, BOTTOM
				is begin
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							insert_silk_arc (module_cursor, face, log_threshold);
							
						when LAYER_CAT_ASSY =>
							insert_doc_arc (module_cursor, face, log_threshold);

						when LAYER_CAT_STENCIL =>
							insert_stencil_arc (module_cursor, face, log_threshold);
							
						when LAYER_CAT_STOPMASK =>
							insert_stop_arc (module_cursor, face, log_threshold);

						when others => null;  -- CS raise exception ?
					end case;
				end insert_arc;



				
				
				procedure insert_circle (
					layer_cat	: in type_layer_category;
					face		: in et_pcb_sides.type_face) -- TOP, BOTTOM
				is begin
					case layer_cat is
						when LAYER_CAT_SILKSCREEN =>
							insert_silk_circle (module_cursor, face, log_threshold);

						when LAYER_CAT_ASSY =>
							insert_doc_circle (module_cursor, face, log_threshold);

						when LAYER_CAT_STENCIL =>
							insert_stencil_circle (module_cursor, face, log_threshold);
							
						when LAYER_CAT_STOPMASK =>
							insert_stop_circle (module_cursor, face, log_threshold);

						when others => null;  -- CS raise exception ?
					end case;
				end insert_circle;



			
				

				
				procedure build_non_conductor_line (
					face : in et_pcb_sides.type_face)
				is begin
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_line (
								layer_cat	=> LAYER_CAT_SILKSCREEN,
								face		=> face);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_line (
								layer_cat	=> LAYER_CAT_ASSY,
								face		=> face);

						when SEC_STENCIL =>
							insert_line (
								layer_cat	=> LAYER_CAT_STENCIL,
								face		=> face);

						when SEC_STOPMASK =>
							insert_line (
								layer_cat	=> LAYER_CAT_STOPMASK,
								face		=> face);

						when SEC_KEEPOUT =>
							insert_line (
								layer_cat	=> LAYER_CAT_KEEPOUT,
								face		=> face);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_line;

				
				
				procedure build_non_conductor_arc (
					face : in et_pcb_sides.type_face)
				is begin					
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_arc (
								layer_cat	=> LAYER_CAT_SILKSCREEN,
								face		=> face);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_arc (
								layer_cat	=> LAYER_CAT_ASSY,
								face		=> face);

						when SEC_STENCIL =>
							insert_arc (
								layer_cat	=> LAYER_CAT_STENCIL,
								face		=> face);

						when SEC_STOPMASK =>
							insert_arc (
								layer_cat	=> LAYER_CAT_STOPMASK,
								face		=> face);

						when SEC_KEEPOUT =>
							insert_arc (
								layer_cat	=> LAYER_CAT_KEEPOUT,
								face		=> face);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_arc;


				
				procedure build_non_conductor_circle (
					face : in et_pcb_sides.type_face)
				is begin
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_circle (
								layer_cat	=> LAYER_CAT_SILKSCREEN,
								face		=> face);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_circle (
								layer_cat	=> LAYER_CAT_ASSY,
								face		=> face);

						when SEC_STENCIL =>
							insert_circle (
								layer_cat	=> LAYER_CAT_STENCIL,
								face		=> face);

						when SEC_STOPMASK =>
							insert_circle (
								layer_cat	=> LAYER_CAT_STOPMASK,
								face		=> face);

						when SEC_KEEPOUT =>
							insert_circle (
								layer_cat	=> LAYER_CAT_KEEPOUT,
								face		=> face);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_circle;							


			
				
				procedure build_non_conductor_cutout (
					face	: in et_pcb_sides.type_face) 
				is begin
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_cutout (
								module_cursor, LAYER_CAT_SILKSCREEN, face, log_threshold);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_cutout (
								module_cursor, LAYER_CAT_ASSY, face, log_threshold);

						when SEC_STENCIL =>
							insert_cutout (
								module_cursor, LAYER_CAT_STENCIL, face, log_threshold);

						when SEC_STOPMASK =>
							insert_cutout (
								module_cursor, LAYER_CAT_STOPMASK, face, log_threshold);

						when SEC_KEEPOUT =>
							insert_cutout (
								module_cursor, LAYER_CAT_KEEPOUT, face, log_threshold);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_cutout;


				
				procedure build_non_conductor_fill_zone (
					face	: in et_pcb_sides.type_face)
				is begin
					case stack.parent (degree => 2) is
						when SEC_SILKSCREEN =>
							insert_polygon (
								module_cursor, LAYER_CAT_SILKSCREEN, face, log_threshold);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_polygon (
								module_cursor, LAYER_CAT_ASSY, face, log_threshold);

						when SEC_STENCIL =>
							insert_polygon (
								module_cursor, LAYER_CAT_STENCIL, face, log_threshold);

						when SEC_STOPMASK =>
							insert_polygon (
								module_cursor, LAYER_CAT_STOPMASK, face, log_threshold);

						when SEC_KEEPOUT =>
							insert_polygon (
								module_cursor, LAYER_CAT_KEEPOUT, face, log_threshold);
							
						when others => invalid_section;
					end case;
				end build_non_conductor_fill_zone;


				
		
				
				procedure build_net_label is
				begin
					case stack.parent is
						when SEC_LABELS =>
							insert_net_label;

						when others => invalid_section;
					end case;
				end build_net_label;
					
				
				
				
			begin -- execute_section
				case stack.current is

					when SEC_CONNECTORS =>
						case stack.parent is
							when SEC_SEGMENT =>
								assign_net_connectors (log_threshold);

							when others => invalid_section;
						end case;

					
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_ZONE | SEC_CUTOUT_ZONE =>
								check_contour (log_threshold);
							
							when others => invalid_section;
						end case;


					when SEC_JUNCTIONS =>
						case stack.parent is
							when SEC_SEGMENT =>
								assign_net_junctions (log_threshold);

							when others => invalid_section;
						end case;
						
						
					when SEC_NET_CLASS =>
						case stack.parent is
							when SEC_NET_CLASSES =>
								assign_net_class (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_NET_CLASSES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_BOARD_LAYER_STACK =>
						case stack.parent is
							when SEC_INIT =>
								add_board_layer (module_cursor, log_threshold);

							when others => invalid_section;
						end case;

						
					when SEC_DRAWING_GRID =>
						case stack.parent is
							when SEC_INIT => 
								set_drawing_grid (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_NET =>
						case stack.parent is
							when SEC_NETS =>
								assign_net (module_cursor, log_threshold);
						
							when others => invalid_section;
						end case;

						
					when SEC_NETS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_STRANDS =>
						case stack.parent is
							when SEC_NET =>
								insert_strands;

							when others => invalid_section;
						end case;

						
					when SEC_ROUTE =>
						case stack.parent is
							when SEC_NET =>
								assign_route;
								
							when others => invalid_section;
						end case;

						
					when SEC_STRAND =>
						case stack.parent is
							when SEC_STRANDS =>
								assign_net_strand (line, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_SEGMENTS =>
						case stack.parent is
							when SEC_STRAND =>
								insert_net_segments;
								
							when others => invalid_section;
						end case;

						
					when SEC_SEGMENT =>
						case stack.parent is
							when SEC_SEGMENTS =>
								assign_net_segment (log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_LABELS =>
						case stack.parent is
							when SEC_SEGMENT =>
								assign_net_labels (log_threshold);

							when others => invalid_section;
						end case;

						
					when SEC_PORTS =>
						case stack.parent is
							when SEC_SEGMENT =>
								assign_net_ports (log_threshold);

							when SEC_SUBMODULE =>
								assign_submodule_ports;
								
							when others => invalid_section;
						end case;

						
					when SEC_LABEL =>
						build_net_label;

						
					when SEC_LINE =>
						case stack.parent is
							when SEC_CONTOURS => insert_contour_line;
								
							when SEC_ROUTE =>
								insert_track_line (module_cursor, log_threshold);

							when SEC_TOP =>
								build_non_conductor_line (et_pcb_sides.TOP);

							when SEC_BOTTOM =>
								build_non_conductor_line (et_pcb_sides.BOTTOM);

							when SEC_ROUTE_RESTRICT =>
								insert_restrict_line (module_cursor, log_threshold);

							when SEC_CONDUCTOR =>
								insert_freetrack_line (module_cursor, log_threshold);

							when SEC_OUTLINE | SEC_HOLE =>
								insert_outline_line;
								
							when others => invalid_section;
						end case;
						
						
					when SEC_ARC =>
						case stack.parent is
							when SEC_CONTOURS => insert_contour_arc;

							when SEC_ROUTE =>
								insert_track_arc (module_cursor, log_threshold);
								
							when SEC_TOP =>
								build_non_conductor_arc (et_pcb_sides.TOP);

							when SEC_BOTTOM =>
								build_non_conductor_arc (et_pcb_sides.BOTTOM);

							when SEC_ROUTE_RESTRICT =>
								insert_restrict_arc (module_cursor, log_threshold);

							when SEC_CONDUCTOR =>
								insert_freetrack_arc (module_cursor, log_threshold);

							when SEC_OUTLINE | SEC_HOLE =>
								insert_outline_arc;
								
							when others => invalid_section;
						end case;

						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_CONTOURS => insert_contour_circle;
							
							when SEC_TOP =>
								build_non_conductor_circle (et_pcb_sides.TOP);

							when SEC_BOTTOM =>
								build_non_conductor_circle (et_pcb_sides.BOTTOM);

							when SEC_ROUTE_RESTRICT =>
								insert_restrict_circle (module_cursor, log_threshold);

							when SEC_CONDUCTOR =>
								insert_freetrack_circle (module_cursor, log_threshold);

							when SEC_OUTLINE | SEC_HOLE =>
								insert_outline_circle;
								
							when others => invalid_section;
						end case;

						
					when SEC_VIA =>
						case stack.parent is
							when SEC_ROUTE =>
								build_via;
						
							when others => invalid_section;
						end case;

						
					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							--when SEC_ROUTE =>
								--build_route_cutout;

							when SEC_TOP =>
								build_non_conductor_cutout (et_pcb_sides.TOP);

							when SEC_BOTTOM =>
								build_non_conductor_cutout (et_pcb_sides.BOTTOM);

							when SEC_ROUTE_RESTRICT =>
								insert_cutout_route_restrict (module_cursor, log_threshold);

							when SEC_VIA_RESTRICT =>
								insert_cutout_via_restrict (module_cursor, log_threshold);

							when SEC_CONDUCTOR =>
								insert_cutout_conductor (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_ZONE =>
						case stack.parent is
							when SEC_ROUTE =>
								build_route_polygon (module_cursor, log_threshold);

							when SEC_TOP =>
								build_non_conductor_fill_zone (et_pcb_sides.TOP);
						
							when SEC_BOTTOM =>
								build_non_conductor_fill_zone (et_pcb_sides.BOTTOM);

							when SEC_ROUTE_RESTRICT =>
								insert_polygon_route_restrict (module_cursor, log_threshold);

							when SEC_VIA_RESTRICT =>
								insert_zone_via_restrict (module_cursor, log_threshold);

							when SEC_CONDUCTOR =>
								insert_polygon_conductor (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_SUBMODULE =>
						case stack.parent is
							when SEC_SUBMODULES =>
								insert_submodule (module_cursor, log_threshold);

							when others => invalid_section;
						end case;

						
					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS =>
								case stack.parent (degree => 2) is
									when SEC_SUBMODULE => 
										insert_submodule_port (line);
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
					when SEC_SUBMODULES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_SCHEMATIC =>
						case stack.parent is
							when SEC_DRAWING_FRAMES =>
								set_frame_schematic (module_cursor, log_threshold);

							when SEC_DRAWING_GRID => null; -- nothing to do

							when SEC_META =>
								add_meta_schematic;

							when others => invalid_section;
						end case;

						
					when SEC_BOARD =>
						case stack.parent is
							when SEC_INIT => null;

							when SEC_DRAWING_FRAMES =>
								set_frame_board (module_cursor, log_threshold);
								
							when SEC_DRAWING_GRID => null; -- nothing to do

							when SEC_META =>
								add_meta_board;
							
							when others => invalid_section;
						end case;

						
					when SEC_SHEET_DESCRIPTIONS =>
						case stack.parent is
							when SEC_SCHEMATIC => null; 
								-- We assign the sheet_descriptions once parent 
								-- section SCHEMATIC closes.
								-- See procdure set_frame_schematic.

							when others => invalid_section;
						end case;

						
					when SEC_SHEET =>
						case stack.parent is
							when SEC_SHEET_DESCRIPTIONS => add_sheet_description;
							when others => invalid_section;
						end case;

						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS =>
								insert_schematic_text (module_cursor, log_threshold);

							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>								
										build_non_conductor_text (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_SILKSCREEN,
											face			=> et_pcb_sides.TOP,
											log_threshold	=> log_threshold);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										build_non_conductor_text (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_ASSY,
											face			=> et_pcb_sides.TOP,
											log_threshold	=> log_threshold);
										
									when SEC_STOPMASK =>
										build_non_conductor_text (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_STOPMASK,
											face			=> et_pcb_sides.TOP,
											log_threshold	=> log_threshold);
										
									when others => invalid_section;
								end case;

						
							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>								
										build_non_conductor_text (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_SILKSCREEN,
											face			=> et_pcb_sides.BOTTOM,
											log_threshold	=> log_threshold);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										build_non_conductor_text (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_ASSY,
											face			=> et_pcb_sides.BOTTOM,
											log_threshold	=> log_threshold);
										
									when SEC_STOPMASK =>
										build_non_conductor_text (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_STOPMASK,
											face			=> et_pcb_sides.BOTTOM,
											log_threshold	=> log_threshold);
										
									when others => invalid_section;
								end case;


							when SEC_CONDUCTOR =>
								build_conductor_text (module_cursor, log_threshold);

							when others => invalid_section;
						end case;

						
					when SEC_TEXTS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_DRAWING_FRAMES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								case stack.parent (degree => 2) is
									when SEC_DEVICE =>
										et_module_read_device_non_electrical.insert_package_placeholder;
										
									when SEC_PACKAGE =>
										et_module_read_device_electrical.insert_package_placeholder;

									when SEC_UNIT =>
										build_unit_placeholder;

									when others => invalid_section;
								end case;

								
							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>
										insert_placeholder (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_SILKSCREEN,											
											face			=> et_pcb_sides.TOP,
											log_threshold	=> log_threshold);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_placeholder (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_ASSY,
											face			=> et_pcb_sides.TOP,
											log_threshold	=> log_threshold);

									when SEC_STOPMASK =>
										insert_placeholder (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_STOPMASK,
											face			=> et_pcb_sides.TOP,
											log_threshold	=> log_threshold);

									when others => invalid_section;
								end case;

								
							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>
										insert_placeholder (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_SILKSCREEN,											
											face			=> et_pcb_sides.BOTTOM,
											log_threshold	=> log_threshold);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_placeholder (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_ASSY,
											face			=> et_pcb_sides.BOTTOM,
											log_threshold	=> log_threshold);

									when SEC_STOPMASK =>
										insert_placeholder (
											module_cursor	=> module_cursor,
											layer_cat		=> LAYER_CAT_STOPMASK,
											face			=> et_pcb_sides.BOTTOM,
											log_threshold	=> log_threshold);

									when others => invalid_section;
								end case;

								
							when SEC_CONDUCTOR =>
								insert_board_text_placeholder (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_PLACEHOLDERS =>
						case stack.parent is
							when SEC_PACKAGE =>
								et_module_read_device_electrical.insert_placeholders;
								

							when SEC_DEVICE =>
								case stack.parent (degree => 2) is
									when SEC_DEVICES_NON_ELECTRIC =>
										et_module_read_device_non_electrical.insert_placeholders;

									when others => invalid_section;
								end case;
								
							when SEC_UNIT => null;
								
							when others => invalid_section;
						end case;

						
					when SEC_PACKAGE =>
						case stack.parent is
							when SEC_DEVICE =>
								set_package_position;

							when others => invalid_section;
						end case;

						
					when SEC_UNIT =>
						case stack.parent is
							when SEC_UNITS =>
								insert_unit (module_cursor, log_threshold);
													
							when others => invalid_section;
						end case;

						
					when SEC_UNITS =>
						case stack.parent is
							when SEC_DEVICE =>
								insert_units;
								
							when others => invalid_section;
						end case;

						
					when SEC_DEVICE =>
						case stack.parent is
							when SEC_DEVICES =>
								insert_device (module_cursor, log_threshold);

							when SEC_DEVICES_NON_ELECTRIC => 
								insert_device_non_electrical (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_DEVICES =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_ASSEMBLY_VARIANT =>
						case stack.parent is
							when SEC_ASSEMBLY_VARIANTS => 
								insert_assembly_variant (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_ASSEMBLY_VARIANTS =>
						case stack.parent is
							when SEC_INIT => null; -- CS test if active variant exists
							when others => invalid_section;
						end case;

						
					when SEC_META =>
						case stack.parent is
							when SEC_INIT => 
								set_meta (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_PREFERRED_LIBRARIES =>
						case stack.parent is
							when SEC_SCHEMATIC =>
								case stack.parent (degree => 2) is
									when SEC_META	=> null; -- nothing to do
									when others		=> invalid_section;
								end case;

							when SEC_BOARD =>
								case stack.parent (degree => 2) is
									when SEC_META	=> null; -- nothing to do
									when others		=> invalid_section;
								end case;
								
							when others => invalid_section;
						end case;

						
					when SEC_RULES =>
						case stack.parent is
							when SEC_INIT => 
								set_rules (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_NETCHANGERS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_NETCHANGER =>
						case stack.parent is
							when SEC_NETCHANGERS =>
								insert_netchanger (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_DEVICES_NON_ELECTRIC | SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
						SEC_STOPMASK | SEC_KEEPOUT | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT |
						SEC_CONDUCTOR | SEC_PCB_CONTOURS_NON_PLATED =>
						case stack.parent is
							when SEC_BOARD => null;
							when others => invalid_section;
						end case;

						
					when SEC_TOP | SEC_BOTTOM =>
						case stack.parent is
							when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
								SEC_STOPMASK | SEC_KEEPOUT => null;

							when others => invalid_section;
						end case;

						
					when SEC_USER_SETTINGS =>
						case stack.parent is
							when SEC_BOARD => 
								assign_user_settings_board (module_cursor, log_threshold);
							-- CS when SEC_SCHEMATIC	=> null;
							
							when others => invalid_section;
						end case;

						
					when SEC_VIAS | SEC_FILL_ZONES_CONDUCTOR =>
						case stack.parent is
							when SEC_USER_SETTINGS =>
								case stack.parent (degree => 2) is
									when SEC_BOARD	=> null;
									when others		=> invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
					when SEC_OUTLINE =>
						case stack.parent is
							when SEC_PCB_CONTOURS_NON_PLATED =>
								set_outline (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_HOLE =>
						case stack.parent is
							when SEC_PCB_CONTOURS_NON_PLATED =>
								add_hole (module_cursor, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_INIT => null; -- CS: should never happen
				end case;

	-- CS:
	-- 				exception when event:
	-- 					others => 
	-- 						log (text => ada.exceptions.exception_message (event), console => true);
	-- 						raise;
				
			end execute_section;


			
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
			function set (
				section_keyword	: in string; -- [NETS
				section			: in type_module_section) -- SEC_NETS
				return boolean is 
			begin -- set
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 5);
						return true;

					elsif f (line, 2) = section_end then -- section footer detected in field 2

						-- The section name in the footer must match the name
						-- of the current section. Otherwise abort.
						if section /= stack.current then
							log_indentation_reset;
							invalid_section;
						end if;
						
						-- Now that the section ends, the data collected in temporarily
						-- variables is processed.
						log_indentation_up;
						execute_section;
						log_indentation_down;
						
						stack.pop;
						if stack.empty then
							log (text => write_top_level_reached, level => log_threshold + 5);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 5);
						end if;
						return true;

					else
						log (ERROR, write_missing_begin_end, console => true);
						raise constraint_error;
					end if;

				else -- neither a section header nor footer
					return false;
				end if;
			end set;


			use et_package_sections;
			use et_device_sections;
		
			
		begin -- process_line
			if set (section_net_classes, SEC_NET_CLASSES) then null;
			elsif set (section_connectors, SEC_CONNECTORS) then null;
			elsif set (section_net_class, SEC_NET_CLASS) then null;
			elsif set (section_junctions, SEC_JUNCTIONS) then null;
			elsif set (section_board_layer_stack, SEC_BOARD_LAYER_STACK) then null;			
			elsif set (section_drawing_grid, SEC_DRAWING_GRID) then null;
			elsif set (section_nets, SEC_NETS) then null;
			elsif set (section_net, SEC_NET) then null;
			elsif set (section_strands, SEC_STRANDS) then null;
			elsif set (section_strand, SEC_STRAND) then null;
			elsif set (section_segments, SEC_SEGMENTS) then null;
			elsif set (section_segment, SEC_SEGMENT) then null;
			elsif set (section_labels, SEC_LABELS) then null;
			elsif set (section_label, SEC_LABEL) then null;
			elsif set (section_fill_zones_conductor, SEC_FILL_ZONES_CONDUCTOR) then null;
			elsif set (section_ports, SEC_PORTS) then null;
			elsif set (section_port, SEC_PORT) then null;				
			elsif set (section_route, SEC_ROUTE) then null;								
			elsif set (section_line, SEC_LINE) then null;								
			elsif set (section_arc, SEC_ARC) then null;
			elsif set (section_cutout_zone, SEC_CUTOUT_ZONE) then null;
			elsif set (section_zone, SEC_ZONE) then null;								
			elsif set (section_contours, SEC_CONTOURS) then null;								
			elsif set (section_via, SEC_VIA) then null;								
			elsif set (section_submodules, SEC_SUBMODULES) then null;
			elsif set (section_submodule, SEC_SUBMODULE) then null;
			elsif set (section_drawing_frames, SEC_DRAWING_FRAMES) then null;
			elsif set (section_schematic, SEC_SCHEMATIC) then null;
			elsif set (section_sheet_descriptions, SEC_SHEET_DESCRIPTIONS) then null;
			elsif set (section_sheet, SEC_SHEET) then null;
			elsif set (section_board, SEC_BOARD) then null;
			elsif set (section_devices, SEC_DEVICES) then null;
			elsif set (section_device, SEC_DEVICE) then null;
			elsif set (section_devices_non_electric, SEC_DEVICES_NON_ELECTRIC) then null;
			elsif set (section_units, SEC_UNITS) then null;
			elsif set (section_unit, SEC_UNIT) then null;
			elsif set (section_assembly_variants, SEC_ASSEMBLY_VARIANTS) then null;
			elsif set (section_assembly_variant, SEC_ASSEMBLY_VARIANT) then null;
			elsif set (section_netchangers, SEC_NETCHANGERS) then null;
			elsif set (section_netchanger, SEC_NETCHANGER) then null;
			elsif set (section_meta, SEC_META) then null;
			elsif set (section_preferred_libraries, SEC_PREFERRED_LIBRARIES) then null;
			elsif set (section_rules, SEC_RULES) then null;			
			elsif set (section_placeholders, SEC_PLACEHOLDERS) then null;				
			elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
			elsif set (section_package, SEC_PACKAGE) then null;
			elsif set (section_texts, SEC_TEXTS) then null;
			elsif set (section_text, SEC_TEXT) then null;
			elsif set (section_silkscreen, SEC_SILKSCREEN) then null;
			elsif set (section_top, SEC_TOP) then null;
			elsif set (section_bottom, SEC_BOTTOM) then null;
			elsif set (section_circle, SEC_CIRCLE) then null;
			elsif set (section_assembly_doc, SEC_ASSEMBLY_DOCUMENTATION) then null;
			elsif set (section_stencil, SEC_STENCIL) then null;
			elsif set (section_stopmask, SEC_STOPMASK) then null;
			elsif set (section_keepout, SEC_KEEPOUT) then null;
			elsif set (section_route_restrict, SEC_ROUTE_RESTRICT) then null;
			elsif set (section_via_restrict, SEC_VIA_RESTRICT) then null;
			elsif set (section_conductor, SEC_CONDUCTOR) then null;				
			elsif set (section_pcb_contours, SEC_PCB_CONTOURS_NON_PLATED) then null;
			elsif set (section_hole, SEC_HOLE) then null;
			elsif set (section_outline, SEC_OUTLINE) then null;
			elsif set (section_user_settings, SEC_USER_SETTINGS) then null;
			elsif set (section_vias, SEC_VIAS) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "module line --> " & to_string (line), level => log_threshold + 4);
				log_indentation_up;

				
				case stack.current is

					when SEC_CONNECTORS =>
						case stack.parent is
							when SEC_SEGMENT => read_net_connector (line, log_threshold);
							when others => invalid_section;
						end case;
					
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_ZONE => null;
							when SEC_CUTOUT_ZONE => null;
							when others => invalid_section;
						end case;

					when SEC_JUNCTIONS =>
						case stack.parent is
							when SEC_SEGMENT => read_net_junction (line, log_threshold);
							when others => invalid_section;
						end case;
						
					when SEC_NET_CLASSES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_DRAWING_GRID =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

					when SEC_BOARD_LAYER_STACK =>
						case stack.parent is
							when SEC_INIT => read_layer (line, log_threshold);
							when others => invalid_section;
						end case;
						
					when SEC_DEVICES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_ASSEMBLY_VARIANTS =>
						case stack.parent is
							when SEC_INIT => 
								set_active_assembly_variant (module_cursor, line, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_ASSEMBLY_VARIANT =>
						case stack.parent is
							when SEC_ASSEMBLY_VARIANTS => 
								read_assembly_variant (module_cursor, line);
								
							when others => invalid_section;
						end case;

						
					when SEC_TEXTS =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do								
							when others => invalid_section;
						end case;

						
					when SEC_SUBMODULES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do								
							when others => invalid_section;
						end case;

						
					when SEC_DRAWING_FRAMES =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do								
							when others => invalid_section;
						end case;

						
					when SEC_META =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_PREFERRED_LIBRARIES =>
						case stack.parent is
							when SEC_SCHEMATIC =>
								case stack.parent (degree => 2) is
									when SEC_META => 
										read_preferred_lib_schematic (line);
										
									when others => invalid_section;
								end case;

							when SEC_BOARD =>
								case stack.parent (degree => 2) is
									when SEC_META =>
										read_preferred_lib_board (line);
										
									when others	=> invalid_section;
								end case;
								
							when others => invalid_section;
						end case;

						
					when SEC_RULES =>
						case stack.parent is
							when SEC_INIT => read_rules (line);
							when others => invalid_section;
						end case;
			
			
					when SEC_NET_CLASS =>
						case stack.parent is
							when SEC_NET_CLASSES => read_net_class (line, log_threshold);
							when others => invalid_section;
						end case;

						
					when SEC_STRAND =>
						case stack.parent is
							when SEC_STRANDS => read_strand (line, log_threshold);
							when others => invalid_section;
						end case;

						
					when SEC_STRANDS =>
						case stack.parent is
							when SEC_NET => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_ROUTE =>
						case stack.parent is
							when SEC_NET => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_NET =>
						case stack.parent is
							when SEC_NETS => read_net (line, log_threshold);
							when others => invalid_section;
						end case;

					when SEC_NETS =>
						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;
						
					when SEC_SEGMENT =>
						case stack.parent is
							when SEC_SEGMENTS => read_net_segment (line, log_threshold);
							when others => invalid_section;
						end case;

					when SEC_SEGMENTS =>
						case stack.parent is
							when SEC_STRAND => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_LABELS =>
						case stack.parent is
							when SEC_SEGMENT => null; -- nothing to do
							when others => invalid_section;
						end case;
						
					when SEC_PORTS =>
						case stack.parent is 
							when SEC_SEGMENT => read_net_port (line, log_threshold);
							when SEC_SUBMODULE => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_LABEL =>
						case stack.parent is
							when SEC_LABELS => read_label (line, log_threshold);
							when others => invalid_section;
						end case;

						
					when SEC_LINE =>
						case stack.parent is
							when SEC_CONTOURS => 
								read_contour_line (line);
								
							when SEC_ROUTE =>
								read_track_line (line);
								
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_ASSEMBLY_DOCUMENTATION =>
										read_doc_line (line);

									when SEC_SILKSCREEN =>
										read_silk_line (line);

									when SEC_STOPMASK =>
										read_stop_line (line);
										
									when SEC_STENCIL =>
										read_stencil_line (line);
										
									when others => invalid_section;
								end case;								
								
							when SEC_ROUTE_RESTRICT =>
								et_module_read_route_restrict.read_restrict_line (line);
								
							when SEC_CONDUCTOR =>
								read_freetrack_line (line);
								
							when SEC_OUTLINE | SEC_HOLE =>
								read_contour_line (line);
								
							when others => invalid_section;
						end case;
						
						
					when SEC_ARC =>
						case stack.parent is
							when SEC_CONTOURS => 
								read_contour_arc (line);
							
							when SEC_ROUTE =>
								read_track_arc (line);
								
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_ASSEMBLY_DOCUMENTATION =>
										read_doc_arc (line);

									when SEC_SILKSCREEN =>
										read_silk_arc (line);

									when SEC_STOPMASK =>
										read_stop_arc (line);
										
									when SEC_STENCIL =>
										read_stencil_arc (line);
										
									when others => invalid_section;
								end case;
								
							when SEC_ROUTE_RESTRICT =>
								et_module_read_route_restrict.read_restrict_arc (line);
									
							when SEC_CONDUCTOR =>
								read_freetrack_arc (line);
								
							when SEC_OUTLINE | SEC_HOLE =>
								read_contour_arc (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_CONTOURS =>
								read_contour_circle (line);
							
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_ASSEMBLY_DOCUMENTATION =>
										read_doc_circle (line);

									when SEC_SILKSCREEN =>
										read_silk_circle (line);

									when SEC_STOPMASK =>
										read_stop_circle (line);
										
									when SEC_STENCIL =>
										read_stencil_circle (line);
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT =>
								et_module_read_route_restrict.read_restrict_circle (line);
																
							when SEC_CONDUCTOR =>
								read_freetrack_circle (line);

							when SEC_OUTLINE | SEC_HOLE =>
								read_contour_circle (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_ROUTE => read_cutout_route (line);
							
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK => 
										read_cutout_non_conductor (line);

									when SEC_KEEPOUT =>
										read_keepout_cutout (line);
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT => 
								read_cutout_restrict (line);
								
							when SEC_CONDUCTOR => 
								read_cutout_conductor_non_electric (line);
							when others => invalid_section;
						end case;

						
					when SEC_ZONE =>
						case stack.parent is
							when SEC_ROUTE => read_fill_zone_route (line);

							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK => 
											read_fill_zone_non_conductor (line);

									when SEC_KEEPOUT => 
										read_fill_zone_keepout (line);
										
									when others => invalid_section;
								end case;

							when SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								read_fill_zone_restrict (line);
							
							when SEC_CONDUCTOR => 
								read_fill_zone_conductor_non_electric (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_VIA =>
						case stack.parent is
							when SEC_ROUTE	=> read_via (module_cursor, line);
							when others		=> invalid_section;
						end case;

						
					when SEC_SUBMODULE =>
						case stack.parent is
							when SEC_SUBMODULES => 
								read_submodule (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_PORT =>
						case stack.parent is
							when SEC_PORTS =>
								case stack.parent (degree => 2) is
									when SEC_SUBMODULE => 
										read_submodule_port (line);
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
					when SEC_SCHEMATIC =>
						case stack.parent is
							when SEC_DRAWING_FRAMES => 
								read_frame_template_schematic (line);
							
							when SEC_DRAWING_GRID => 
								read_drawing_grid_schematic (line);
								
							when SEC_META => read_meta_schematic (line);
							when others => invalid_section;
						end case;

						
					when SEC_BOARD =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							
							when SEC_DRAWING_FRAMES => 
								read_frame_template_board (line);
							
							when SEC_DRAWING_GRID => 
								read_drawing_grid_board (line);
								
							when SEC_META => read_meta_board (line);
							when others => invalid_section;
						end case;

						
					when SEC_SHEET_DESCRIPTIONS =>
						case stack.parent is
							when SEC_SCHEMATIC => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_SHEET =>
						case stack.parent is
							when SEC_SHEET_DESCRIPTIONS => 
								read_sheet_description (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_TEXT =>
						case stack.parent is
							when SEC_TEXTS => -- in schematic
								read_schematic_text (line);

							when SEC_PCB_CONTOURS_NON_PLATED => -- in board
								read_board_text_contours (line);
								
							when SEC_TOP | SEC_BOTTOM => -- in board
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION 
										| SEC_STOPMASK | SEC_KEEPOUT | SEC_STENCIL =>	
											read_board_text_non_conductor (line);

									when others => invalid_section;
								end case;

								
							when SEC_CONDUCTOR | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
								read_board_text_conductor (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_DEVICE =>
						case stack.parent is
							when SEC_DEVICES => read_device (line);
							when SEC_DEVICES_NON_ELECTRIC => read_device_non_electric (line);
							when others => invalid_section;
						end case;

						
					when SEC_PACKAGE =>
						case stack.parent is
							when SEC_DEVICE =>
								et_module_read_device_electrical.read_package_position (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_PLACEHOLDERS =>
								case stack.parent (degree => 2) is
									when SEC_DEVICE => 
										et_module_read_device_non_electrical.read_device_text_placeholder (line);
									
									when SEC_PACKAGE => 
										et_module_read_device_electrical.read_device_text_placeholder (line);
										
									when SEC_UNIT => read_unit_placeholder (line);
									when others => invalid_section;
								end case;

								
							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION 
										| SEC_STOPMASK => -- CS SEC_KEEPOUT
										read_board_text_placeholder (line);
							
									when others => invalid_section;
								end case;

								
							when SEC_CONDUCTOR =>
								read_board_text_conductor_placeholder (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_PLACEHOLDERS =>
						case stack.parent is
							when SEC_PACKAGE => null;

							when SEC_DEVICE =>
								case stack.parent (degree => 2) is
									when SEC_DEVICES_NON_ELECTRIC => null;

									when others => invalid_section;
								end case;
							
							when SEC_UNIT => null;
							when others => invalid_section;
						end case;

						
					when SEC_UNIT =>
						case stack.parent is
							when SEC_UNITS => read_unit (line);
							when others => invalid_section;
						end case;

						
					when SEC_UNITS =>
						case stack.parent is
							when SEC_DEVICE => null;
							when others => invalid_section;
						end case;

						
					when SEC_NETCHANGERS =>
						case stack.parent is
							when SEC_INIT => null; -- nothing to do
							when others => invalid_section;
						end case;

						
					when SEC_NETCHANGER =>
						case stack.parent is
							when SEC_NETCHANGERS =>
								read_netchanger (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_DEVICES_NON_ELECTRIC | SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
						SEC_STOPMASK | SEC_KEEPOUT | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT |
						SEC_CONDUCTOR | SEC_PCB_CONTOURS_NON_PLATED =>
						case stack.parent is
							when SEC_BOARD => null;
							when others => invalid_section;
						end case;

						
					when SEC_TOP | SEC_BOTTOM =>
						case stack.parent is
							when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION | SEC_STENCIL |
								SEC_STOPMASK | SEC_KEEPOUT => null;

							when others => invalid_section;
						end case;

						
					when SEC_USER_SETTINGS =>
						case stack.parent is
							when SEC_BOARD		=> null;
							-- CS when SEC_SCHEMATIC	=> null;
							
							when others => invalid_section;
						end case;

						
					when SEC_VIAS =>
						case stack.parent is
							when SEC_USER_SETTINGS =>
								case stack.parent (degree => 2) is
									when SEC_BOARD =>
										read_user_settings_vias (line);
										
									when others		=> invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
					when SEC_FILL_ZONES_CONDUCTOR =>
						case stack.parent is
							when SEC_USER_SETTINGS =>
								case stack.parent (degree => 2) is
									when SEC_BOARD =>
										read_user_settings_fill_zones_conductor (line);
										
									when others		=> invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
					when SEC_OUTLINE | SEC_HOLE =>
						case stack.parent is
							when SEC_PCB_CONTOURS_NON_PLATED => null;
							when others => invalid_section;
						end case;

						
					when SEC_INIT => null; -- CS: should never happen
				end case;


				log_indentation_down;
			end if;

			exception when event: others =>
				log (text => "file " & file_name & space 
					& get_affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;




		
		-- This procdure reads the module file line per line
		-- and adds the content to the module. Cursor module_cursor
		-- points to the module:
		procedure read_module_file is begin

			-- Open the module file and direct the input accordingly:
			open (
				file => file_handle,
				mode => in_file, 
				name => file_name_expanded);
			
			set_input (file_handle);
			
			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);

			
			-- Read the file line by line:
			while not end_of_file loop
				line := et_string_processing.read_line (
					line 			=> get_line,
					number			=> positive (ada.text_io.line (current_input)),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- We are interested only in lines that 
				-- contain something. emtpy lines are skipped:
				if get_field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			
			-- As a safety measure the top section must be reached finally:
			if stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			-- Close the module file and set the input back to standard input:
			set_input (previous_input);
			close (file_handle);

			-- Compute the ratsnest (airwires):
			et_board_ops.ratsnest.update_ratsnest (module_cursor, log_threshold + 2);
		end read_module_file;



		
		
		procedure read_submodule_files is
		-- Pointer module_cursor points to the last module that has been read.
		-- Take a copy of the submodules stored in module.submods. 
		-- Then iterate in that copy (submods) to read the actual 
		-- module files (like templates/clock_generator.mod).
		-- NOTE: The parent procedure "read_module" calls itself here !

			use et_submodules;
			use pac_submodules;

			-- Here the copy of submodules lives:
			submods : et_submodules.pac_submodules.map;

			
			procedure get_submodules (
			-- Copies the submodules in submods.
				module_name	: pac_module_name.bounded_string;
				module		: type_generic_module) is
			begin
				submods := module.submods;
			end;

			
			procedure query_module (cursor : in pac_submodules.cursor) is begin
				-- Read the template file:
				read_module (to_string (element (cursor).file), log_threshold + 1);
			end;

			
		begin -- read_submodule_files
			-- take a copy of submodules
			query_element (
				position	=> module_cursor,
				process		=> get_submodules'access);

			if get_count (submods) > 0 then
				log (text => "submodules/templates ...", level => log_threshold);
				log_indentation_up;
			
				-- Query submodules of the parent module (accessed by module_cursor):
				iterate (submods, query_module'access);

				log_indentation_down;
			end if;

		end read_submodule_files;



		
		use ada.directories;
		
		
	begin
		log (text => "opening module file " & enclose_in_quotes (file_name) & " ...", level => log_threshold);
		--log (text => "full name " & enclose_in_quotes (file_name_expanded), level => log_threshold + 1);
		log_indentation_up;
		
		-- Make sure the module file exists.
		-- The file_name may contain environment variables (like $templates). 
		-- In order to test whether the given module file exists, file name_name must be expanded
		-- so that the environment variables are replaced by the real paths like:
		-- templates/clock_generator.mod or
		-- /home/user/et_templates/pwr_supply.mod.
		if exists (file_name_expanded) then

			log (text => "expanded name: " & enclose_in_quotes (full_name (file_name_expanded)),
					level => log_threshold + 1);
			
			-- Create an empty module named after the module file (omitting extension *.mod).
			-- So the module names are things like "motor_driver", "templates/clock_generator" or
			-- "$TEMPLATES/clock_generator" or "/home/user/templates/clock_generator":
			pac_generic_modules.insert (
				container	=> generic_modules,
				key			=> to_module_name (remove_extension (file_name)),
				position	=> module_cursor,
				inserted	=> module_inserted);

			
			-- If the module is new to the collection of generic modules,
			-- then open the module file file and read it. 
			-- Otherwise notify operator that module has already been loaded.			 
			if module_inserted then

				read_module_file;
						
				-- Pointer module_cursor points to the last module that has been read.		
				-- The names of submodule/template files are stored in module.submods.file.
				-- But the files itself have not been read. That is what we do next:
				read_submodule_files;

				-- Test existence of assembly variants of submodules.
				test_assembly_variants_of_submodules (module_cursor, log_threshold + 1);
				
			else
				log (text => "module " & enclose_in_quotes (file_name) &
					" already loaded -> no need to load anew.", level => log_threshold + 1);
			end if;


			
		else -- module file not found
			raise semantic_error_1 with
				"ERROR: Module file " & enclose_in_quotes (file_name) & " not found !";
		end if;

		
		log_indentation_down;
		
		exception when event: others =>
			if is_open (file_handle) then close (file_handle); end if;
			set_input (previous_input);
			raise;
		
	end read_module;


	
	
	
end et_module_read;
	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
