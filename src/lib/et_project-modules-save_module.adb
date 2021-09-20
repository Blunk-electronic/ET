------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             SAVE MODULE                                  --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 - 2021 Mario Blunk, Blunk electronic          --
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


separate (et_project.modules)

procedure save_module (
	module_cursor	: in pac_generic_modules.cursor;
	save_as_name	: in pac_module_name.bounded_string := to_module_name (""); -- motor_driver_test, templates/clock_generator_test
	log_threshold	: in type_log_level)
is
	use et_conductor_text;
	use pac_generic_modules;

	-- backup the previous output destination
	previous_output : ada.text_io.file_type renames current_output;
	
	module_file_handle : ada.text_io.file_type;

	procedure write_header is 
	-- Creates the module/submodule file and writes a nice header in it.
		use ada.directories;
		use gnat.directory_operations;
		use pac_project_name;
		use type_et_project_path;
		use et_general;
	begin
		-- Compose the target full file name and create the module file:
		
		if pac_module_name.length (save_as_name) = 0 then
			-- The module is to be saved with its own name:

			log (text => "Saving module as " 
				 & enclose_in_quotes (to_string (key (module_cursor))) & " ...",
				 level => log_threshold + 1);
				 
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
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " " & row_separator_double);
		new_line;
	end write_header;		

	procedure write_footer is
	-- writes a nice footer in the module file and closes it.
	begin
		new_line;
		
		log (text => "closing module file ...", level => log_threshold + 1);
		
		put_line (comment_mark & " " & row_separator_double);
		put_line (comment_mark & " " & date);
		put_line (comment_mark & " module file end");
		new_line;

		set_output (previous_output);
		close (module_file_handle);
	end write_footer;
	
	function rotation (pos : in et_pcb_coordinates.pac_geometry_brd.type_position'class)  -- CS make generic ?
		return string
	is
		use et_pcb_coordinates.pac_geometry_brd;
	begin
		return to_string (rot (pos));
	end rotation;
	
	function face (point : et_pcb_coordinates.type_package_position) return string is
		use et_pcb_coordinates;
	begin
		return to_string (get_face (point));
	end face;

	procedure query_meta is
		use et_meta;
		meta : et_meta.type_meta := element (module_cursor).meta;

		procedure write_basic (basic : in type_basic'class) is begin
			write (keyword => keyword_company, parameters => to_string (basic.company), wrap => true);
			write (keyword => keyword_customer, parameters => to_string (basic.customer), wrap => true);
			write (keyword => keyword_partcode, parameters => to_string (basic.partcode));
			write (keyword => keyword_drawing_number, parameters => to_string (basic.drawing_number));
			write (keyword => keyword_revision, parameters => to_string (basic.revision));
			
			write (keyword => keyword_drawn_by, parameters => to_string (basic.drawn_by), wrap => true);
			write (keyword => keyword_drawn_date, parameters => to_string (basic.drawn_date));
			
			write (keyword => keyword_checked_by, parameters => to_string (basic.checked_by), wrap => true);
			write (keyword => keyword_checked_date, parameters => to_string (basic.checked_date));

			write (keyword => keyword_approved_by, parameters => to_string (basic.approved_by), wrap => true);
			write (keyword => keyword_approved_date, parameters => to_string (basic.approved_date));
		end write_basic;

		procedure write_schematic (sch : in type_schematic) is 
			use pac_preferred_libraries_schematic;
			
			procedure query_lib (c : in pac_preferred_libraries_schematic.cursor) is begin
				write (keyword => keyword_path, parameters => to_string (element (c)));
			end query_lib;
	
		begin -- write_schematic
			section_mark (section_preferred_libraries, HEADER);
			sch.preferred_libs.iterate (query_lib'access);
			section_mark (section_preferred_libraries, FOOTER);
		end write_schematic;

		procedure write_board (brd : in type_board) is
			use pac_preferred_libraries_board;
			
			procedure query_lib (c : in pac_preferred_libraries_board.cursor) is begin
				write (keyword => keyword_path, parameters => to_string (element (c)));
			end query_lib;

		begin -- write_board
			section_mark (section_preferred_libraries, HEADER);
			brd.preferred_libs.iterate (query_lib'access);
			section_mark (section_preferred_libraries, FOOTER);
		end write_board;

		
	begin -- query_meta
		log_indentation_up;
		log (text => "meta data ...", level => log_threshold + 1);
		
		section_mark (section_meta, HEADER);

		-- schematic related
		section_mark (section_schematic, HEADER);
		write_basic (meta.schematic);
		write_schematic (meta.schematic);
		
		section_mark (section_schematic, FOOTER);

		
		-- board related
		section_mark (section_board, HEADER);
		write_basic (meta.board);
		write_board (meta.board);
		section_mark (section_board, FOOTER);
		
		section_mark (section_meta, FOOTER);

		log_indentation_down;
	end query_meta;

	procedure query_rules is
		use et_design_rules;
		use et_schematic;
		rules : constant type_rules := element (module_cursor).rules;
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
		use et_pcb;
		use et_pcb.pac_net_classes;
		use et_pcb_coordinates.pac_geometry_brd;

		procedure write (class_cursor : in pac_net_classes.cursor) is begin
			log (text => "net class " & to_string (key (class_cursor)), level => log_threshold + 1);
			section_mark (section_net_class, HEADER);

			write (keyword => keyword_name, parameters => to_string (key (class_cursor)));
			write (keyword => keyword_description, parameters => et_pcb.to_string (element (class_cursor).description), wrap => true);
			write (keyword => keyword_clearance, parameters => to_string (element (class_cursor).clearance));
			write (keyword => keyword_track_width_min, parameters => to_string (element (class_cursor).track_width_min));
			write (keyword => keyword_via_drill_min, parameters => to_string (element (class_cursor).via_drill_min));
			write (keyword => keyword_via_restring_min, parameters => to_string (element (class_cursor).via_restring_min));
			write (keyword => keyword_micro_via_drill_min, parameters => to_string (element (class_cursor).micro_via_drill_min));
			write (keyword => keyword_micro_via_restring_min, parameters => to_string (element (class_cursor).micro_via_restring_min));

			section_mark (section_net_class, FOOTER);
		end write;
	
	begin -- query_net_classes
		log_indentation_up;
		
		section_mark (section_net_classes, HEADER);
		iterate (element (module_cursor).net_classes, write'access);
		section_mark (section_net_classes, FOOTER);

		log_indentation_down;
	end query_net_classes;

	procedure query_drawing_grid is 
		use et_coordinates.pac_geometry_sch;
		use et_pcb_coordinates.pac_geometry_brd;
	begin
		log_indentation_up;
		
		section_mark (section_drawing_grid, HEADER);

		section_mark (section_schematic, HEADER);
		write (keyword => keyword_default, parameters => 
				keyword_x & to_string (element (module_cursor).grid.x) & space &
				keyword_y & to_string (element (module_cursor).grid.y));
		section_mark (section_schematic, FOOTER);

		section_mark (section_board, HEADER);
		write (keyword => keyword_default, parameters => 
				keyword_x & to_string (element (module_cursor).board.grid.x) & space &
				keyword_y & to_string (element (module_cursor).board.grid.y));
		section_mark (section_board, FOOTER);
		
		section_mark (section_drawing_grid, FOOTER);

		log_indentation_down;
	end query_drawing_grid;

	procedure query_layer_stack is
		use et_pcb_coordinates.pac_geometry_brd;
		use et_pcb_stack;
		use package_layers;

		procedure query_layers (cursor : in package_layers.cursor) is
			layer : type_layer := element (cursor);
		begin
			-- write: "conductor   1 0.035"
			write (keyword => keyword_conductor,
					parameters => 2 * space & to_string (to_index (cursor)) & to_string (layer.conductor.thickness));

			-- write "dielectric  1 0.200"
			write (keyword => keyword_dielectric, 
					parameters => space & to_string (to_index (cursor)) & to_string (layer.dielectric.thickness));

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
		
	procedure query_nets is
		use et_symbols.pac_text;
		use type et_schematic.type_net;
		use et_schematic.pac_nets;
		use et_pcb;

		procedure query_strands (
			net_name	: in pac_net_name.bounded_string;
			net			: in et_schematic.type_net) is
			use et_schematic;
			use pac_strands;
			use et_coordinates.pac_geometry_sch;
			strand_cursor : pac_strands.cursor := net.strands.first;

			procedure query_segments (strand : in type_strand) is
				use pac_net_segments;
				segment_cursor : pac_net_segments.cursor := strand.segments.first;

				use pac_device_ports;
				use pac_submodule_ports;

				use et_netlists;
				use pac_netchanger_ports;
				
				procedure query_labels (segment : in type_net_segment) is
					use pac_net_labels;
					label_cursor : pac_net_labels.cursor := segment.labels.first;
				begin
					if not is_empty (segment.labels) then
						section_mark (section_labels, HEADER);
						while label_cursor /= pac_net_labels.no_element loop
							section_mark (section_label, HEADER);
							
							write (keyword => keyword_position, parameters => position (element (label_cursor).position));

							case element (label_cursor).appearance is
								when SIMPLE =>
									-- The simple label can be read from the front or from the right:
									write (keyword => keyword_rotation, parameters => 
										to_string (element (label_cursor).rotation_simple));

								when TAG =>
									-- The tag label ran be rotated arbitrary:
									write (keyword => keyword_rotation, parameters =>
										to_string (element (label_cursor).rotation_tag));
							end case;
							
							write (keyword => et_text.keyword_size, parameters => to_string (element (label_cursor).size));

							write (keyword => keyword_appearance, parameters =>
								et_schematic.to_string (appearance => element (label_cursor).appearance));
							
							-- a tag label also indicates a signal direction
							if element (label_cursor).appearance = TAG then
								write (keyword => et_schematic.keyword_direction, parameters => to_string (element (label_cursor).direction));
							end if;
							
							section_mark (section_label, FOOTER);
							next (label_cursor);
						end loop;
						section_mark (section_labels, FOOTER);
					end if;
				end query_labels;

				procedure query_junctions (segment : in type_net_segment) is begin
					if segment.junctions.start_point then
						write (keyword => keyword_junction, parameters => keyword_start);
					end if;

					if segment.junctions.end_point then
						write (keyword => keyword_junction, parameters => keyword_end);
					end if;
				end query_junctions;
				
				procedure query_device_ports (segment : in type_net_segment) is
					use et_symbols;
					port_cursor : pac_device_ports.cursor := segment.ports_devices.first;
				begin -- query_device_ports
					while port_cursor /= pac_device_ports.no_element loop
						write (keyword => keyword_device, parameters => 
							space & to_string (element (port_cursor).device_name)
							& space & keyword_port & space
							& to_string (element (port_cursor).port_name)
							); -- device IC1 port A
						next (port_cursor);
					end loop;
				end query_device_ports;

				procedure query_submodule_ports (segment : in type_net_segment) is
					use et_symbols;
					port_cursor : pac_submodule_ports.cursor := segment.ports_submodules.first;
				begin
					while port_cursor /= pac_submodule_ports.no_element loop

						write (keyword => keyword_submodule, parameters => 
							space & to_string (element (port_cursor).module_name)
							& space & keyword_port & space
							& to_string (element (port_cursor).port_name)
							); -- submodule CLK_GENERATOR port out

						next (port_cursor);
					end loop;
				end query_submodule_ports;
				
				procedure query_netchanger_ports (segment : in type_net_segment) is
					use et_symbols;
					port_cursor : pac_netchanger_ports.cursor := segment.ports_netchangers.first;
				begin
					while port_cursor /= pac_netchanger_ports.no_element loop

						write (keyword => keyword_netchanger, parameters => 
							et_submodules.to_string (element (port_cursor).index)
							& space & keyword_port
							& et_submodules.to_string (element (port_cursor).port)
							); -- netchanger 1 port master/slave

						next (port_cursor);
					end loop;
				end query_netchanger_ports;

			begin -- query_segments
				section_mark (section_segments, HEADER);
				while segment_cursor /= pac_net_segments.no_element loop
					section_mark (section_segment, HEADER);

					write (keyword => keyword_start, parameters => position (element (segment_cursor).start_point));
					write (keyword => keyword_end,   parameters => "  " & position (element (segment_cursor).end_point));

					query_element (segment_cursor, query_labels'access);
					query_element (segment_cursor, query_junctions'access);

					-- write ports there are any. otherwise leave out section ports.
					if 	is_empty (element (segment_cursor).ports_devices) and
						is_empty (element (segment_cursor).ports_submodules) and
						is_empty (element (segment_cursor).ports_netchangers) then
						null;
					else
						section_mark (section_ports, HEADER);
						query_element (segment_cursor, query_device_ports'access);
						query_element (segment_cursor, query_submodule_ports'access);
						query_element (segment_cursor, query_netchanger_ports'access);
						section_mark (section_ports, FOOTER);
					end if;
						
					section_mark (section_segment, FOOTER);
					next (segment_cursor);
				end loop;
				section_mark (section_segments, FOOTER);
			end query_segments;
			
		begin -- query_strands
			section_mark (section_strands, HEADER);
			while strand_cursor /= pac_strands.no_element loop
				section_mark (section_strand, HEADER);

				write (keyword => keyword_position, parameters => et_schematic_rw.position (element (strand_cursor).position));

				query_element (strand_cursor, query_segments'access);
				
				section_mark (section_strand, FOOTER);
				next (strand_cursor);
			end loop;
			
			section_mark (section_strands, FOOTER);
		end query_strands;

		procedure query_route (
		-- This is about routed tracks/traces and zones in the board:
			net_name	: in pac_net_name.bounded_string;
			net			: in et_schematic.type_net) 
		is
			--use et_packages;
			use et_board_shapes_and_text;
			use pac_shapes;

			use et_terminals;
			use et_pcb;
			use et_pcb_stack;
			use et_pcb_coordinates.pac_geometry_brd;
			
			use pac_conductor_lines;
			line_cursor : pac_conductor_lines.cursor := net.route.lines.first;

			use pac_conductor_arcs;
			arc_cursor : pac_conductor_arcs.cursor := net.route.arcs.first;

			use et_conductor_polygons.pac_signal_polygons_solid; 
			use et_conductor_polygons.pac_signal_polygons_hatched;
			use et_conductor_polygons.pac_conductor_cutouts;
			polygon_solid_cursor	: et_conductor_polygons.pac_signal_polygons_solid.cursor := net.route.polygons.solid.first;
			polygon_hatched_cursor	: et_conductor_polygons.pac_signal_polygons_hatched.cursor := net.route.polygons.hatched.first;
			cutout_zone_cursor		: et_conductor_polygons.pac_conductor_cutouts.cursor := net.route.cutouts.first;

			procedure write_vias is
				use et_vias;
				use pac_vias;

				procedure query_via (c : in pac_vias.cursor) is begin
					section_mark (section_via, HEADER);

					write (keyword => keyword_via_category, parameters => to_string (element (c).category));
					write (keyword => keyword_position, parameters => position (element (c).position));
					write (keyword => keyword_diameter, parameters => to_string (element (c).diameter));

					case element (c).category is
						when THROUGH =>
							write (keyword => keyword_restring_outer,
								parameters => to_string (element (c).restring_outer));
							
						when BLIND_DRILLED_FROM_TOP =>
							write (keyword => keyword_restring_outer, parameters => to_string (element (c).restring_top));
							write (keyword => keyword_destination, parameters => to_string (element (c).lower));
							
						when BLIND_DRILLED_FROM_BOTTOM =>
							write (keyword => keyword_restring_outer, parameters => to_string (element (c).restring_bottom));
							write (keyword => keyword_destination, parameters => to_string (element (c).upper));
							
						when BURIED =>
							write (keyword => et_vias.keyword_layers, parameters => to_string (element (c).layers));
					end case;

					write (keyword => keyword_restring_inner,
						parameters => to_string (element (c).restring_inner));
					
					section_mark (section_via, FOOTER);
				end query_via;
				
			begin
				net.route.vias.iterate (query_via'access);
			end write_vias;
			
		begin -- query_route
			section_mark (section_route, HEADER);

			while line_cursor /= pac_conductor_lines.no_element loop
				section_mark (section_line, HEADER);
				
				write (keyword => keyword_start, parameters => position (element (line_cursor).start_point));
				write (keyword => keyword_end  , parameters => position (element (line_cursor).end_point));
				write (keyword => keyword_layer, parameters => to_string (element (line_cursor).layer));
				write (keyword => et_pcb_rw.keyword_width, parameters => to_string (element (line_cursor).width));

				section_mark (section_line, FOOTER);
				next (line_cursor);
			end loop;

			while arc_cursor /= pac_conductor_arcs.no_element loop
				section_mark (section_arc, HEADER);

				write (keyword => keyword_center, parameters => position (element (arc_cursor).center));
				write (keyword => keyword_start , parameters => position (element (arc_cursor).start_point));
				write (keyword => keyword_end   , parameters => position (element (arc_cursor).end_point));
				write (keyword => et_pcb_rw.keyword_width , parameters => to_string (element (arc_cursor).width));
				write (keyword => keyword_layer , parameters => to_string (element (arc_cursor).layer));
				
				section_mark (section_arc, FOOTER);
				next (arc_cursor);
			end loop;

			write_vias;
			
			-- solid fill zones
			while polygon_solid_cursor /= et_conductor_polygons.pac_signal_polygons_solid.no_element loop
				fill_zone_begin;

				write_easing (element (polygon_solid_cursor).easing);
				
				write_width_min (element (polygon_solid_cursor).width_min);
				write_isolation (element (polygon_solid_cursor).isolation);
				
				write_priority (element (polygon_solid_cursor).properties.priority_level);
				write_signal_layer (element (polygon_solid_cursor).properties.layer);

				write_fill_style (SOLID);

				case element (polygon_solid_cursor).connection is
					when et_conductor_polygons.THERMAL => 
						write_pad_connection (element (polygon_solid_cursor).connection);
						write_thermal (element (polygon_solid_cursor).thermal);
		
					when et_conductor_polygons.SOLID =>
						write_pad_technology (element (polygon_solid_cursor).technology);
						
				end case;

				contours_begin;
				write_polygon_segments (pac_shapes.type_polygon_base (element (polygon_solid_cursor)));
				contours_end;
				
				fill_zone_end;
				next (polygon_solid_cursor);
			end loop;

			-- hatched fill zones
			while polygon_hatched_cursor /= et_conductor_polygons.pac_signal_polygons_hatched.no_element loop
				fill_zone_begin;

				write_easing (element (polygon_hatched_cursor).easing);

				write_width_min (element (polygon_hatched_cursor).width_min);
				write_isolation (element (polygon_hatched_cursor).isolation);
				
				write_priority (element (polygon_hatched_cursor).properties.priority_level);
				write_signal_layer (element (polygon_hatched_cursor).properties.layer);

				write_fill_style (HATCHED);
				
				write_hatching (element (polygon_hatched_cursor).hatching);

				case element (polygon_hatched_cursor).connection is
					when et_conductor_polygons.THERMAL => 
						write_pad_connection (element (polygon_hatched_cursor).connection);
						write_thermal (element (polygon_hatched_cursor).thermal);
		
					when et_conductor_polygons.SOLID =>
						write_pad_technology (element (polygon_hatched_cursor).technology);

				end case;

				contours_begin;
				write_polygon_segments (pac_shapes.type_polygon_base (element (polygon_hatched_cursor)));
				contours_end;
				
				fill_zone_end;
				next (polygon_hatched_cursor);
			end loop;

			-- cutout zones
			while cutout_zone_cursor /= et_conductor_polygons.pac_conductor_cutouts.no_element loop
				cutout_zone_begin;
				write_signal_layer (element (cutout_zone_cursor).layer);

				contours_begin;
				write_polygon_segments (pac_shapes.type_polygon_base (element (cutout_zone_cursor)));
				contours_end;
				
				cutout_zone_end;
				next (cutout_zone_cursor);
			end loop;
			
			section_mark (section_route, FOOTER);
		end query_route;

		procedure write (net_cursor : in et_schematic.pac_nets.cursor) is begin
			log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
			section_mark (section_net, HEADER);

			write (keyword => keyword_name, parameters => to_string (key (net_cursor)));
			write (keyword => keyword_class, parameters => to_string (element (net_cursor).class));
			write (keyword => keyword_scope, parameters => et_netlists.to_string (element (net_cursor).scope));

			query_element (net_cursor, query_strands'access);
			query_element (net_cursor, query_route'access);
			
			section_mark (section_net, FOOTER);
			new_line;
		end write;
		
	begin -- query_nets
		log_indentation_up;

		section_mark (section_nets, HEADER);
		iterate (element (module_cursor).nets, write'access);
		section_mark (section_nets, FOOTER);
		
		log_indentation_down;
	end query_nets;

	procedure query_devices is		
		use et_schematic;
		use et_symbols;
		use pac_devices_sch;

		procedure query_units (device_name : in type_device_name; device : in type_device_sch) is
			use et_schematic.pac_units;
			unit_cursor : pac_units.cursor := device.units.first;

			use et_coordinates.pac_geometry_sch;
			
			procedure write_placeholder (ph : in type_text_placeholder) is begin
				section_mark (section_placeholder, HEADER);
				write (keyword => keyword_meaning, parameters => to_string (ph.meaning));
				write (keyword => keyword_position, parameters => position (ph.position));
				write_text_properties (ph);
				section_mark (section_placeholder, FOOTER);
			end write_placeholder;

			use et_devices;
			
		begin -- query_units
			section_mark (section_units, HEADER);
			while unit_cursor /= pac_units.no_element loop
				section_mark (section_unit, HEADER);
				write (keyword => keyword_name, parameters => to_string (key (unit_cursor)));
				write (keyword => keyword_position, parameters => position (element (unit_cursor).position)); -- position sheet 1 x 147.32 y 96.97
				--write (keyword => keyword_rotation, parameters => to_string (element (unit_cursor).rotation)); -- rotation 180.0
				write (keyword => keyword_rotation, parameters => to_string (rot (element (unit_cursor).position))); -- rotation 180.0
				write (keyword => keyword_mirrored, parameters => to_string (element (unit_cursor).mirror, verbose => false)); -- x_axis, y_axis, none

				if element (unit_cursor).appearance = PCB then
					section_mark (section_placeholders, HEADER);
					
					write_placeholder (element (unit_cursor).name);
					write_placeholder (element (unit_cursor).value);
					write_placeholder (element (unit_cursor).purpose);
					--write_placeholder (element (unit_cursor).partcode);

					section_mark (section_placeholders, FOOTER);
				end if;
				
				section_mark (section_unit, FOOTER);
				next (unit_cursor);
			end loop;
			section_mark (section_units, FOOTER);
		end query_units;

		procedure query_placeholders (
			device_name : in type_device_name;
			device 		: in type_device_sch) is
			use et_pcb_coordinates;
			use et_packages;
			use pac_text_placeholders;

			face : et_pcb_coordinates.type_face;
			layer : type_placeholder_package_layer;
			
			procedure write_placeholder (placeholder_cursor : in pac_text_placeholders.cursor) is 
			begin
				section_mark (section_placeholder, HEADER);
				write (keyword => et_pcb_stack.keyword_layer, parameters => to_string (layer));
				write (keyword => keyword_meaning, parameters => to_string (element (placeholder_cursor).meaning));
				write_text_properties_with_face (element (placeholder_cursor), face);
				section_mark (section_placeholder, FOOTER);
			end write_placeholder;
			
		begin -- query_placeholders
			section_mark (section_placeholders, HEADER);

			layer := SILK_SCREEN;
			face := TOP;
			device.text_placeholders.silk_screen.top.iterate (write_placeholder'access);

			face := BOTTOM;				
			device.text_placeholders.silk_screen.bottom.iterate (write_placeholder'access);

			layer := ASSEMBLY_DOCUMENTATION;
			face := TOP;				
			device.text_placeholders.assy_doc.top.iterate (write_placeholder'access);

			face := BOTTOM;
			device.text_placeholders.assy_doc.bottom.iterate (write_placeholder'access);
			
			section_mark (section_placeholders, FOOTER);				
		end query_placeholders;

		procedure write (d : in pac_devices_sch.cursor) is 
			use et_material;
		begin
			section_mark (section_device, HEADER);
			write (keyword => keyword_name, parameters => to_string (key (d)));
			write (keyword => keyword_appearance, parameters => to_string (element (d).appearance));
			write (keyword => keyword_model, parameters => to_string (element (d).model));

			case element (d).appearance is
				when PCB =>
					-- write the value if a value exists for the device:
					if not is_empty (element (d).value) then
						write (keyword => keyword_value, parameters => to_string (element (d).value));
					end if;
					
					write (keyword => keyword_variant , parameters => to_string (element (d).variant));

					-- write the partcode if a partcode exists for the device;
					if not is_empty (element (d).partcode) then
						write (keyword => keyword_partcode, parameters => to_string (element (d).partcode));
					end if;

					-- write the purpose if a purpose exists for the device;
					if not is_empty (element (d).purpose) then
						write (keyword => keyword_purpose , parameters => to_string (element (d).purpose), wrap => true);
					end if;
					
					section_mark (section_package, HEADER);

					-- Flip status:
					write (keyword => keyword_flipped, parameters => et_pcb.to_string (element (d).flipped));
					
					-- This is the position of the package in the layout, 
					write (keyword => keyword_position, parameters => -- position x 34.5 y 60.1 face top/bottom
							position (element (d).position));
				
					query_element (d, query_placeholders'access);
					section_mark (section_package, FOOTER);
					
				when VIRTUAL => null;
			end case;

			query_element (d, query_units'access);
			
			section_mark (section_device, FOOTER);
			new_line;
		end write;
		
	begin -- query_devices
		section_mark (section_devices, HEADER);
		iterate (element (module_cursor).devices, write'access);
		section_mark (section_devices, FOOTER);
	end query_devices;

	procedure query_assembly_variants is
	-- writes the assembly variants in the module file
		use et_assembly_variants;
		use et_assembly_variants.pac_assembly_variants;

		procedure query_devices (
			variant_name	: in et_general.pac_assembly_variant_name.bounded_string;
			variant			: in et_assembly_variants.type_assembly_variant) is
			use et_assembly_variants.pac_device_variants;
			device_cursor : et_assembly_variants.pac_device_variants.cursor := variant.devices.first;

			function purpose return string is 
				use et_devices;
				use pac_device_purpose;
			begin
				if length (element (device_cursor).purpose) > 0 then
					return space & keyword_purpose & space &
						enclose_in_quotes (
							text_in => et_devices.to_string (element (device_cursor).purpose),
							quote	=> latin_1.quotation);
				else
					return "";
				end if;
			end;

			use et_devices;
			
		begin -- query_devices
			while device_cursor /= et_assembly_variants.pac_device_variants.no_element loop
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
								space & et_material.keyword_partcode & space &
								et_material.to_string (element (device_cursor).partcode) &
								purpose);

				end case;
				
				next (device_cursor);
			end loop;
		end query_devices;

		procedure query_submodules (
			variant_name	: in et_general.pac_assembly_variant_name.bounded_string;
			variant			: in et_assembly_variants.type_assembly_variant) is
			use et_assembly_variants;
			use et_assembly_variants.pac_submodule_variants;
			submodule_cursor : et_assembly_variants.pac_submodule_variants.cursor := variant.submodules.first;
		begin
			while submodule_cursor /= pac_submodule_variants.no_element loop
				write (
					keyword		=> keyword_submodule,
					parameters	=> et_general.to_string (key (submodule_cursor)) &
									space & keyword_variant & space &
									to_variant (element (submodule_cursor).variant));
				
				next (submodule_cursor);
			end loop;
		end query_submodules;

		procedure write (variant_cursor : in et_assembly_variants.pac_assembly_variants.cursor) is begin
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
				parameters	=> et_general.to_variant (element (module_cursor).active_variant));

		end if;
		
		section_mark (section_assembly_variants, FOOTER);
	end query_assembly_variants;
	
	procedure query_netchangers is
	-- writes the netchangers in the module file
		use et_submodules;
		use pac_netchangers;

		procedure query_netchanger (cursor : pac_netchangers.cursor) is
		begin
			section_mark (section_netchanger, HEADER);
			write (keyword => keyword_name,	parameters => to_string (key (cursor))); -- 1, 2, 201, ...
			write (keyword => keyword_position_in_schematic, parameters => position (element (cursor).position_sch)); -- position_in_schematic sheet 1 x 147.32 y 96.97
			write (keyword => keyword_rotation_in_schematic, parameters => pac_geometry_sch.to_string (pac_geometry_sch.rot (element (cursor).position_sch))); -- rotation_in_schematic 90.0
			write (keyword => keyword_position_in_board, parameters => position (element (cursor).position_brd)); -- position_in_board x 1.32 y 6.97
			write (keyword => et_pcb_stack.keyword_layer, parameters => et_pcb_stack.to_string (element (cursor).layer)); -- layer 2
			section_mark (section_netchanger, FOOTER);
		end query_netchanger;
		
	begin
		section_mark (section_netchangers, HEADER);
		iterate (element (module_cursor).netchangers, query_netchanger'access);
		section_mark (section_netchangers, FOOTER);
	end query_netchangers;
	
	procedure query_frames is 
		use et_frames;
		use et_frames.pac_template_name;
		
		procedure write_sheet_descriptions is
			use pac_schematic_descriptions;

			procedure query_sheet (s : in pac_schematic_descriptions.cursor) is
			begin
				section_mark (section_sheet, HEADER);
				write (
					keyword		=> keyword_sheet_number,
					parameters	=> to_sheet (key (s)));

				write (
					keyword		=> keyword_sheet_category,
					parameters	=> to_string (element (s).category));

				write (
					keyword		=> keyword_sheet_description,
					wrap		=> true,
					parameters	=> to_string (element (s).content));
				
				section_mark (section_sheet, FOOTER);
			end query_sheet;
			
		begin -- write_sheet_descriptions
			section_mark (section_sheet_descriptions, HEADER);
			iterate (element (module_cursor).frames.descriptions, query_sheet'access);
			section_mark (section_sheet_descriptions, FOOTER);
		end write_sheet_descriptions;
		
	begin -- query_frames
		-- schematic frames:
		section_mark (section_drawing_frames, HEADER);
		section_mark (section_schematic, HEADER);
		
		-- Write the schematic frame template like "template ../frames/dummy.frs":
		write (
			keyword 	=> keyword_template, 
			parameters	=> et_frames.to_string (element (module_cursor).frames.template));
		
		write_sheet_descriptions;
		
		section_mark (section_schematic, FOOTER);			

		
		-- board frame:
		section_mark (section_board, HEADER);

		-- Write the frame template like "template ../frames/dummy.frb":
		write (
			keyword		=> keyword_template, 
			parameters	=> et_frames.to_string (element (module_cursor).board.frame.template));
			
		-- write the board origin like "origin x 40 y 60"
		write (
			keyword		=> keyword_origin,
			parameters	=> position (element (module_cursor).board.origin));
			
		section_mark (section_board, FOOTER);			
		section_mark (section_drawing_frames, FOOTER);
	end query_frames;

	procedure query_submodules is		
		use et_schematic;
		use et_submodules;
		use pac_submodules;

		procedure query_ports (port_cursor : in et_submodules.pac_submodule_ports.cursor) is
			use et_submodules.pac_submodule_ports;
		begin
			section_mark (section_port, HEADER);
			write (keyword => keyword_name, parameters => to_string (key (port_cursor))); -- name clk_out
			write (keyword => keyword_position, parameters => position (element (port_cursor).position)); -- position x 0 y 10
			write (keyword => et_submodules.keyword_direction, parameters => to_string (element (port_cursor).direction)); -- direction master/slave
			section_mark (section_port, FOOTER);
		end;

		procedure write (submodule_cursor : in pac_submodules.cursor) is 
			use et_coordinates.pac_geometry_sch;
		begin
			section_mark (section_submodule, HEADER);
			write (keyword => keyword_name, parameters => et_general.to_string (key (submodule_cursor))); -- name stepper_driver_1
			write (keyword => keyword_file, parameters => pac_submodule_path.to_string (element (submodule_cursor).file)); -- file $ET_TEMPLATES/motor_driver.mod

			write (keyword => keyword_position, parameters => position (element (submodule_cursor).position));
			write (keyword => et_submodules.keyword_size, parameters => 
				space & keyword_x & to_string (element (submodule_cursor).size.x) &
				space & keyword_y & to_string (element (submodule_cursor).size.y)); -- size x 50 y 70
			
			write (keyword => keyword_position_in_board, parameters => -- position_in_board x 23 y 0.2 rotation 90.0
				position (element (submodule_cursor).position_in_board));

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
		use et_symbols.pac_text;
		use et_coordinates;
		use et_coordinates.pac_geometry_sch;
		use et_schematic;
		use pac_texts;
		
		procedure write (text_cursor : in pac_texts.cursor) is begin
			section_mark (section_text, HEADER);
			write
				(
				keyword		=> keyword_position,
				parameters	=> keyword_sheet & to_sheet (element (text_cursor).sheet) 
								& space & position (element (text_cursor).position)
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
		--use et_packages;
		--use et_terminals;
		use et_board_shapes_and_text;
		use et_pcb;
		use et_pcb_stack;
		use et_pcb_coordinates.pac_geometry_brd;

		--use et_packages.pac_texts_with_content;
		use pac_text_fab;
		use pac_texts_fab_with_content;
		
		use et_pcb.pac_text_placeholders;

		use et_packages.pac_silk_lines;
		use et_packages.pac_silk_arcs;
		use et_packages.pac_silk_circles;
		use et_packages.pac_silk_polygons;

		use et_packages.pac_doc_lines;
		use et_packages.pac_doc_arcs;
		use et_packages.pac_doc_circles;
		use et_packages.pac_doc_polygons;

		use et_stencil;
		use pac_stencil_lines;
		use pac_stencil_arcs;
		use pac_stencil_circles;
		use pac_stencil_polygons;

		use et_stop_mask;
		use pac_stop_lines;
		use pac_stop_arcs;
		use pac_stop_circles;
		use pac_stop_polygons;

		use et_packages.pac_keepout_lines;
		use et_packages.pac_keepout_arcs;
		use et_packages.pac_keepout_circles;
		use et_packages.pac_keepout_polygons;

		use et_route_restrict;
		use pac_route_restrict_lines;
		use pac_route_restrict_arcs;
		use pac_route_restrict_circles;
		use pac_route_restrict_polygons;

		use et_via_restrict;
		use pac_via_restrict_lines;
		use pac_via_restrict_arcs;
		use pac_via_restrict_circles;
		use pac_via_restrict_polygons;

		--use et_packages.pac_conductor_texts;
		use et_conductor_text;
		use pac_conductor_texts_board;
		--use pac_conductor_texts_package;

		-- general stuff
		use pac_text_placeholders_conductors;
		procedure write_placeholder (cursor : in et_pcb.pac_text_placeholders.cursor) is
		begin
			placeholder_begin;
			write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
			write_text_properties (element (cursor));
			placeholder_end;
		end write_placeholder;

		-- CONDUCTOR (NON-ELECTRIC) in any signal layers
		use pac_conductor_lines;
		procedure write_line (cursor : in pac_conductor_lines.cursor) is begin
			line_begin;
			write_line (element (cursor));
			write_width (element (cursor).width);
			write_signal_layer (element (cursor).layer);
			line_end;
		end;

		use pac_conductor_arcs;
		procedure write_arc (cursor : in pac_conductor_arcs.cursor) is begin
			arc_begin;
			write_arc (element (cursor));
			write_width (element (cursor).width);
			write_signal_layer (element (cursor).layer);
			arc_end;
		end;

		use et_pcb.pac_conductor_circles;
		procedure write_circle (cursor : in et_pcb.pac_conductor_circles.cursor) is begin
			write_circle_conductor (element (cursor));
		end;

		-- solid fill zones in conductor
		use et_conductor_polygons.pac_conductor_polygons_floating_solid;
		procedure write_polygon (cursor : in et_conductor_polygons.pac_conductor_polygons_floating_solid.cursor) is begin
			fill_zone_begin;

			write_easing (element (cursor).easing);

			write_width_min (element (cursor).width_min);
			write_isolation (element (cursor).isolation);

			write_priority (element (cursor).properties.priority_level);
			write_signal_layer (element (cursor).properties.layer);

			write_fill_style (element (cursor).fill_style);

			write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));

			fill_zone_end;
		end;

		-- hatched fill zones in conductor
		use et_conductor_polygons.pac_conductor_polygons_floating_hatched;
		procedure write_polygon (cursor : in et_conductor_polygons.pac_conductor_polygons_floating_hatched.cursor) is begin
			fill_zone_begin;

			write_easing (element (cursor).easing);

			write_width_min (element (cursor).width_min);
			write_isolation (element (cursor).isolation);

			write_priority (element (cursor).properties.priority_level);
			write_signal_layer (element (cursor).properties.layer);

			write_fill_style (element (cursor).fill_style);
			write_hatching (element (cursor).hatching);

			write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));

			fill_zone_end;
		end;

		-- cutout zones in any signal layers
		use et_conductor_polygons.pac_conductor_cutouts;
		procedure write_cutout (cursor : in et_conductor_polygons.pac_conductor_cutouts.cursor) is begin
			cutout_zone_begin;
			write_signal_layer (element (cursor).layer);
			write_polygon_segments (pac_shapes.type_polygon_base (element (cursor)));
			cutout_zone_end;
		end;

		-- texts in any signal layers
		procedure write_text (cursor : in pac_conductor_texts_board.cursor) is begin
			text_begin;
			write (keyword => keyword_content, wrap => true,
				   parameters => to_string (element (cursor).content));
			
			write_text_properties (element (cursor));
			write (keyword => keyword_layer, 
				   parameters => to_string (element (cursor).layer));
			text_end;
		end write_text;

		-- text placeholders in any signal layers
		procedure write_placeholder (cursor : in pac_text_placeholders_conductors.cursor) is begin
			placeholder_begin;
			write (keyword => keyword_meaning, parameters => to_string (element (cursor).meaning));
			write_text_properties (element (cursor));
			write (keyword => keyword_layer, parameters => to_string (element (cursor).layer));
			placeholder_end;
		end write_placeholder;

		use et_packages.pac_silk_cutouts;
		use et_packages.pac_doc_cutouts;
		use pac_stencil_cutouts;
		use pac_stop_cutouts;
		use et_packages.pac_keepout_cutouts;

		use pac_route_restrict_cutouts;
		use pac_via_restrict_cutouts;
		use et_schematic.pac_devices_non_electric;
		
		procedure query_devices_non_electric (
			c : in et_schematic.pac_devices_non_electric.cursor) 
		is
			use et_packages;

			procedure query_placeholders (
				device_name : in type_device_name;
				device 		: in et_schematic.type_device_non_electric) 
			is
				use et_pcb_coordinates;
				use et_packages.pac_text_placeholders;

				face : et_pcb_coordinates.type_face;
				layer : et_packages.type_placeholder_package_layer;
				
				procedure write_placeholder (placeholder_cursor : in et_packages.pac_text_placeholders.cursor) is 
				begin
					section_mark (section_placeholder, HEADER);
					write (keyword => et_pcb_stack.keyword_layer, parameters => to_string (layer));
					write (keyword => keyword_meaning, parameters => to_string (element (placeholder_cursor).meaning));
					write_text_properties_with_face (element (placeholder_cursor), face);
					section_mark (section_placeholder, FOOTER);
				end write_placeholder;
				
			begin -- query_placeholders
				section_mark (section_placeholders, HEADER);

				layer := SILK_SCREEN;
				face := TOP;
				device.text_placeholders.silk_screen.top.iterate (write_placeholder'access);

				face := BOTTOM;				
				device.text_placeholders.silk_screen.bottom.iterate (write_placeholder'access);

				layer := ASSEMBLY_DOCUMENTATION;
				face := TOP;				
				device.text_placeholders.assy_doc.top.iterate (write_placeholder'access);

				face := BOTTOM;
				device.text_placeholders.assy_doc.bottom.iterate (write_placeholder'access);
				
				section_mark (section_placeholders, FOOTER);				
			end query_placeholders;
				
		begin -- query_devices_non_electric
			section_mark (section_device, HEADER);

			write (keyword => keyword_name, parameters => to_string (key (c))); -- name FD1
			write (keyword => keyword_flipped, parameters => to_string (element (c).flipped));
			write (keyword => keyword_position, parameters => position (element (c).position));
			write (keyword => keyword_model, parameters => to_string (element (c).package_model));

			query_element (c, query_placeholders'access);
			
			section_mark (section_device, FOOTER);
		end query_devices_non_electric;

		procedure query_user_settings is
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
						keyword		=> keyword_via_restring_inner, 
						parameters	=> to_string (us.vias.restring_inner.width));
				else
					write ( -- restring_inner dru
						keyword		=> keyword_via_restring_inner, 
						parameters	=> keyword_dru);
				end if;

				-- outer restring
				if us.vias.restring_outer.active then
					write ( -- restring_outer 0.3
						keyword		=> keyword_via_restring_outer, 
						parameters	=> to_string (us.vias.restring_outer.width));
				else
					write ( -- restring_inner dru
						keyword		=> keyword_via_restring_outer, 
						parameters	=> keyword_dru);
				end if;
				
				section_mark (section_vias, FOOTER);
			end vias;

			procedure polygons is begin
				section_mark (section_fill_zones_conductor, HEADER);

				write_fill_style (us.polygons_conductor.fill_style);
				write_width_min (us.polygons_conductor.min_width);
				write_priority (us.polygons_conductor.priority_level);
				write_isolation (us.polygons_conductor.isolation);
				
				write_hatching (us.polygons_conductor.hatching);
				
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
		


		procedure write_board_contours is 
			use et_packages;
			use pac_pcb_cutouts;
			
			procedure write_hole (c : in pac_pcb_cutouts.cursor) is begin
				section_mark (section_hole, HEADER);		
				write_polygon_segments (element (c));		
				section_mark (section_hole, FOOTER);		
			end write_hole;

			use pac_text_fab.pac_texts_fab_with_content;
		begin
			section_mark (section_pcb_contours, HEADER);

			-- board outline and texts
			section_mark (section_outline, HEADER);
			write_polygon_segments (element (module_cursor).board.contours.outline);
			iterate (element (module_cursor).board.contours.texts, write_text'access);
			section_mark (section_outline, FOOTER);

			-- holes
			iterate (element (module_cursor).board.contours.holes, write_hole'access);
			
			section_mark (section_pcb_contours, FOOTER);
		end write_board_contours;
	
		
	begin -- query_board
		section_mark (section_board, HEADER);

		-- USER SETTINGS
		query_user_settings;
	
		-- NON-ELECTRIC DEVICES
		section_mark (section_devices_non_electric, HEADER);
		iterate (element (module_cursor).devices_non_electric, query_devices_non_electric'access);
		section_mark (section_devices_non_electric, FOOTER);

		-- SILK SCREEN
		section_mark (section_silk_screen, HEADER);

			section_mark (section_top, HEADER);
			iterate (element (module_cursor).board.silk_screen.top.lines, write_line'access);
			iterate (element (module_cursor).board.silk_screen.top.arcs, write_arc'access);
			iterate (element (module_cursor).board.silk_screen.top.circles, write_circle'access);
			iterate (element (module_cursor).board.silk_screen.top.polygons, write_polygon'access);
			iterate (element (module_cursor).board.silk_screen.top.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.silk_screen.top.texts, write_text'access);
			iterate (element (module_cursor).board.silk_screen.top.placeholders, write_placeholder'access);
			section_mark (section_top, FOOTER);

			section_mark (section_bottom, HEADER);
			iterate (element (module_cursor).board.silk_screen.bottom.lines, write_line'access);
			iterate (element (module_cursor).board.silk_screen.bottom.arcs, write_arc'access);
			iterate (element (module_cursor).board.silk_screen.bottom.circles, write_circle'access);
			iterate (element (module_cursor).board.silk_screen.bottom.polygons, write_polygon'access);
			iterate (element (module_cursor).board.silk_screen.bottom.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.silk_screen.bottom.texts, write_text'access);
			iterate (element (module_cursor).board.silk_screen.bottom.placeholders, write_placeholder'access);
			section_mark (section_bottom, FOOTER);
		
		section_mark (section_silk_screen, FOOTER);

		-- ASSEMBLY DOCUMENTATION
		section_mark (section_assembly_doc, HEADER);

		section_mark (section_top, HEADER);
			iterate (element (module_cursor).board.assy_doc.top.lines, write_line'access);
			iterate (element (module_cursor).board.assy_doc.top.arcs, write_arc'access);
			iterate (element (module_cursor).board.assy_doc.top.circles, write_circle'access);
			iterate (element (module_cursor).board.assy_doc.top.polygons, write_polygon'access);
			iterate (element (module_cursor).board.assy_doc.top.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.assy_doc.top.texts, write_text'access);
			iterate (element (module_cursor).board.assy_doc.top.placeholders, write_placeholder'access);
			section_mark (section_top, FOOTER);

			section_mark (section_bottom, HEADER);
			iterate (element (module_cursor).board.assy_doc.bottom.lines, write_line'access);
			iterate (element (module_cursor).board.assy_doc.bottom.arcs, write_arc'access);
			iterate (element (module_cursor).board.assy_doc.bottom.circles, write_circle'access);
			iterate (element (module_cursor).board.assy_doc.bottom.polygons, write_polygon'access);
			iterate (element (module_cursor).board.assy_doc.bottom.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.assy_doc.bottom.texts, write_text'access);
			iterate (element (module_cursor).board.assy_doc.bottom.placeholders, write_placeholder'access);
			section_mark (section_bottom, FOOTER);

		section_mark (section_assembly_doc, FOOTER);

		-- STENCIL
		section_mark (section_stencil, HEADER);

		section_mark (section_top, HEADER);
			iterate (element (module_cursor).board.stencil.top.lines, write_line'access);
			iterate (element (module_cursor).board.stencil.top.arcs, write_arc'access);
			iterate (element (module_cursor).board.stencil.top.circles, write_circle'access);
			iterate (element (module_cursor).board.stencil.top.polygons, write_polygon'access);
			iterate (element (module_cursor).board.stencil.top.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.stencil.top.texts, write_text'access);
			section_mark (section_top, FOOTER);

			section_mark (section_bottom, HEADER);
			iterate (element (module_cursor).board.stencil.bottom.lines, write_line'access);
			iterate (element (module_cursor).board.stencil.bottom.arcs, write_arc'access);
			iterate (element (module_cursor).board.stencil.bottom.circles, write_circle'access);
			iterate (element (module_cursor).board.stencil.bottom.polygons, write_polygon'access);
			iterate (element (module_cursor).board.stencil.bottom.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.stencil.bottom.texts, write_text'access);		
		section_mark (section_bottom, FOOTER);

		section_mark (section_stencil, FOOTER);

		-- STOP MASK
		section_mark (et_pcb_rw.section_stop_mask, HEADER);

		section_mark (section_top, HEADER);
			iterate (element (module_cursor).board.stop_mask.top.lines, write_line'access);
			iterate (element (module_cursor).board.stop_mask.top.arcs, write_arc'access);
			iterate (element (module_cursor).board.stop_mask.top.circles, write_circle'access);
			iterate (element (module_cursor).board.stop_mask.top.polygons, write_polygon'access);
			iterate (element (module_cursor).board.stop_mask.top.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.stop_mask.top.texts, write_text'access);			
			section_mark (section_top, FOOTER);

			section_mark (section_bottom, HEADER);
			iterate (element (module_cursor).board.stop_mask.bottom.lines, write_line'access);
			iterate (element (module_cursor).board.stop_mask.bottom.arcs, write_arc'access);
			iterate (element (module_cursor).board.stop_mask.bottom.circles, write_circle'access);
			iterate (element (module_cursor).board.stop_mask.bottom.polygons, write_polygon'access);
			iterate (element (module_cursor).board.stop_mask.bottom.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.stop_mask.bottom.texts, write_text'access);
			section_mark (section_bottom, FOOTER);

		section_mark (et_pcb_rw.section_stop_mask, FOOTER);

		-- KEEPOUT
		section_mark (section_keepout, HEADER);

		section_mark (section_top, HEADER);
			iterate (element (module_cursor).board.keepout.top.lines, write_line'access);
			iterate (element (module_cursor).board.keepout.top.arcs, write_arc'access);
			iterate (element (module_cursor).board.keepout.top.circles, write_circle'access);
			iterate (element (module_cursor).board.keepout.top.polygons, write_polygon'access);
			iterate (element (module_cursor).board.keepout.top.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.keepout.top.texts, write_text'access);
			section_mark (section_top, FOOTER);

			section_mark (section_bottom, HEADER);
			iterate (element (module_cursor).board.keepout.bottom.lines, write_line'access);
			iterate (element (module_cursor).board.keepout.bottom.arcs, write_arc'access);
			iterate (element (module_cursor).board.keepout.bottom.circles, write_circle'access);
			iterate (element (module_cursor).board.keepout.bottom.polygons, write_polygon'access);
			iterate (element (module_cursor).board.keepout.bottom.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.keepout.bottom.texts, write_text'access);
			section_mark (section_bottom, FOOTER);

		section_mark (section_keepout, FOOTER);

		-- ROUTE RESTRICT
		section_mark (section_route_restrict, HEADER);
			iterate (element (module_cursor).board.route_restrict.lines, write_line'access);
			iterate (element (module_cursor).board.route_restrict.arcs, write_arc'access);
			iterate (element (module_cursor).board.route_restrict.circles, write_circle'access);
			iterate (element (module_cursor).board.route_restrict.polygons, write_polygon'access);
			iterate (element (module_cursor).board.route_restrict.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.route_restrict.texts, write_text'access);
		section_mark (section_route_restrict, FOOTER);

		-- VIA RESTRICT
		section_mark (section_via_restrict, HEADER);
			iterate (element (module_cursor).board.via_restrict.lines, write_line'access);
			iterate (element (module_cursor).board.via_restrict.arcs, write_arc'access);
			iterate (element (module_cursor).board.via_restrict.circles, write_circle'access);
			iterate (element (module_cursor).board.via_restrict.polygons, write_polygon'access);
			iterate (element (module_cursor).board.via_restrict.cutouts, write_cutout'access);
			iterate (element (module_cursor).board.via_restrict.texts, write_text'access);
		section_mark (section_via_restrict, FOOTER);

		-- CONDUCTOR (NON-ELECTRIC)
		section_mark (section_conductor, HEADER);
			iterate (element (module_cursor).board.conductors.lines, write_line'access);
			iterate (element (module_cursor).board.conductors.arcs, write_arc'access);
			iterate (element (module_cursor).board.conductors.circles, write_circle'access);
			iterate (element (module_cursor).board.conductors.polygons.solid, write_polygon'access);
			iterate (element (module_cursor).board.conductors.polygons.hatched, write_polygon'access);
			iterate (element (module_cursor).board.conductors.cutouts, write_cutout'access);			
			iterate (element (module_cursor).board.conductors.texts, write_text'access);
			iterate (element (module_cursor).board.conductors.placeholders, write_placeholder'access);
		section_mark (section_conductor, FOOTER);

		-- BOARD CONTOUR
		write_board_contours;
		
		---BOARD END-----
		section_mark (section_board, FOOTER);
	end query_board;

begin -- save_module

	-- CS check if module is inside project directory ?
	
	write_header;

	-- meta data
	query_meta;
	put_line (row_separator_single);

	-- rules
	query_rules;
	put_line (row_separator_single);
	
	-- net classes
	query_net_classes;
	put_line (row_separator_single);

	-- drawing grid
	query_drawing_grid;
	put_line (row_separator_single);

	-- layer stack
	query_layer_stack;
	put_line (row_separator_single);
	
	-- nets
	query_nets;
	put_line (row_separator_single);
	
	-- frames
	query_frames;
	put_line (row_separator_single);
	
	-- notes
	query_texts;
	put_line (row_separator_single);
	
	-- submodules
	query_submodules;
	put_line (row_separator_single);
	
	-- devices
	query_devices;
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
	
end save_module;


-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
