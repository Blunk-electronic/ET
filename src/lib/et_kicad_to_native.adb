------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                            KICAD TO NATIVE                               --
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
------------------------------------------------------------------------------

--   For correct displaying set tab width in your edtior to 4.

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
with ada.directories;			use ada.directories;
with gnat.directory_operations;
with ada.exceptions; 			use ada.exceptions;
with gnat.source_info;

with et_nets;
with et_net_labels;
with et_net_junction;
with et_net_ports;
with et_net_segment;
with et_net_class;
with et_units;
with et_schematic_text;

with et_mirroring;					use et_mirroring;
with et_axes;						use et_axes;
with et_module_names;				use et_module_names;
with et_module_ops;
with et_string_processing;			use et_string_processing;
with et_project;
with et_generic_module;				use et_generic_module;
with et_vias;
with et_package_names;
with et_packages;
with et_kicad_general;
with et_kicad_libraries;
with et_kicad_packages;
with et_kicad.pcb;
with et_kicad.schematic;
with et_kicad_coordinates;
with et_import;
with et_sheets;
with et_netlists;
with et_text;
with et_pcb_rw;
with et_pcb_rw.device_packages;
with et_pcb_placeholders;
with et_device_rw;
with et_power_sources;
with et_logic;
with et_port_sensitivity;
with et_port_strength;
with et_port_names;
with et_port_direction;
with et_symbol_ports;
with et_symbols;
with et_device_appearance;
with et_device_purpose;
with et_unit_name;					use et_unit_name;
with et_device_model;				use et_device_model;
with et_device_library;				use et_device_library;
with et_package_variant;
with et_device_name;				--use et_device_name;
with et_device_model_names;			use et_device_model_names;
with et_device_prefix;				use et_device_prefix;
with et_device_partcode;			use et_device_partcode;
with et_drawing_frame;	
with et_drawing_frame.schematic;
with et_drawing_frame.board;
with et_fill_zones;					use et_fill_zones;
with et_fill_zones.boards;	
with et_conductor_text;				use et_conductor_text;
with et_conductor_text.boards;
with et_conductor_segment.boards;	use et_conductor_segment.boards;
with et_stopmask;					use et_stopmask;
with et_stencil;					use et_stencil;
with et_silkscreen;					use et_silkscreen;
with et_assy_doc;					use et_assy_doc;
with et_keepout;					use et_keepout;
with et_device_placeholders;		use et_device_placeholders;
with et_schematic_text;
with et_object_status;
with et_devices_electrical;


package body et_kicad_to_native is

	
	procedure transpose (log_threshold : in type_log_level) is
	-- Transposes coordinates of schematic and layout elements:
	-- 1. In schematic changes the path (selector of type_object_position) to the root path (/).
	--    CS: Native coordinates currently do not require the "path" selector. The change-path-stuff is thus not required.
	-- 2. Moves schematic and layout objects from negative to positive y coordinates.
	--    (The origin in kicad is the upper left corner. The origin in ET is the lower left corner.)
		use et_kicad.pcb.type_modules;
		module_cursor : et_kicad.pcb.type_modules.cursor :=
			et_kicad.pcb.type_modules.first (et_kicad.pcb.modules);

		root : et_kicad_coordinates.type_path_to_submodule.list := et_kicad_coordinates.type_path_to_submodule.empty_list;
-- 		before	: constant string (1..15) := "position before";
-- 		now		: constant string (1..15) := "position now   ";
		before	: constant string (1..6) := "before";
		now		: constant string (1..6) := "now   ";

		-- This list of frames serves to map from sheet number to paper size:
		schematic_frames : et_kicad.schematic.type_frames.list;

		-- Here the height of the layout sheet is kept. It is required for move ops of 
		-- layout objects from the kicad frame to the native frame.
		layout_sheet_height : et_board_coordinates.pac_geometry_2.type_distance_positive;

		
		-- Returns true if the current kicad module has a layout file.
		function board_available return boolean is begin
			if element (module_cursor).board_available then
				return true;
			else 
				return false;
			end if;
		end board_available;


		
		-- Returns for a given sheet number the respective paper size.
		function paper_size_of_schematic_sheet (
			sheet_number : in et_sheets.type_sheet)
			return et_drawing_frame.type_paper_size 
		is
			use et_sheets;
			use et_drawing_frame;

			-- This is to be returned. In case no paper size was found, use the default value of type_paper_size.
			size : type_paper_size := paper_size_default;

			sheet_found : boolean := false; -- goes true once the given sheet has been found

			
			procedure query_sheet_number (frame : in et_kicad.schematic.type_frame) is
				use et_schematic_coordinates;
			begin
				if et_kicad_coordinates.sheet (frame.coordinates) = sheet_number then
					size := frame.paper;
					sheet_found := true;
				end if;
			end query_sheet_number;

			-- We search for the paper size in the list "frames":
			use et_kicad.schematic.type_frames;
			frame_cursor : et_kicad.schematic.type_frames.cursor := schematic_frames.first;

			
		begin -- paper_size_of_schematic_sheet

			-- loop in list of frames given in "frames"
			while frame_cursor /= et_kicad.schematic.type_frames.no_element loop
				
				query_element (
					position	=> frame_cursor,
					process		=> query_sheet_number'access);

				if sheet_found then exit; end if; -- cancel search once the given sheet has been found
				
				next (frame_cursor);
			end loop;

			if not sheet_found then
				log (ERROR, "sheet with number" & to_string (sheet_number) & " not found !");
				raise constraint_error;
			end if;
			
			return size;
		end paper_size_of_schematic_sheet;

		
		
		-- Transposes a schematic point from the kicad frame to the ET native frame.
		-- KiCad frames have the origin in the upper left corner.
		-- ET frames have the origin in the lower left corner.
		procedure move (
			point : in out et_kicad_coordinates.type_position) 
		is
			use et_sheets;
			use et_schematic_coordinates;
			use pac_geometry_2;
			
			sheet_number 		: type_sheet;
			sheet_paper_size	: et_drawing_frame.type_paper_size;
			sheet_height		: type_distance_positive;
			new_y				: type_position_axis;
		begin -- move
			-- get the sheet number where the given point resides
			sheet_number		:= et_kicad_coordinates.sheet (point); 

			-- get the paper size of the sheet
			sheet_paper_size	:= paper_size_of_schematic_sheet (sheet_number);

			-- get the paper height of the sheet
			--sheet_height		:= et_schematic_sheets.pac_frames.paper_dimension (axis => Y, paper_size => sheet_paper_size);
			sheet_height		:= type_distance_positive (et_drawing_frame.paper_dimension (
									axis => AXIS_Y, paper_size => sheet_paper_size));

			-- calculate the new y position
			--new_y				:= sheet_height - distance (axis => Y, point => point);
			new_y				:= sheet_height - et_kicad_coordinates.get_y (point);

			-- assign the new y position to the given point
			et_kicad_coordinates.set (point, AXIS_Y, new_y);
		end move;


		
		-- Transposes the schematic point_actual from the kicad frame to the ET native frame.
		-- point_help has supporting purpose: it provides the sheet number where point_actual sits.
		-- KiCad frames have the origin in the upper left corner.
		-- ET frames have the origin in the lower left corner.
		procedure move (
			point_actual	: in out et_schematic_coordinates.pac_geometry_2.type_vector_model;	-- the point it is about
			point_help		: in et_kicad_coordinates.type_position) -- supportive point that provides the sheet number
		is
			use et_sheets;
			use et_schematic_coordinates;
			use pac_geometry_2;
			
			sheet_number 		: type_sheet;
			sheet_paper_size	: et_drawing_frame.type_paper_size;
			sheet_height		: type_distance_positive;
			new_y				: type_position_axis;
		begin -- move
			-- get the sheet number where the given point resides
			sheet_number		:= et_kicad_coordinates.sheet (point_help); 

			-- get the paper size of the sheet
			sheet_paper_size	:= paper_size_of_schematic_sheet (sheet_number);

			-- get the paper height of the sheet
			--sheet_height		:= et_schematic_sheets.pac_frames.paper_dimension (axis => Y, paper_size => sheet_paper_size);
			sheet_height		:= type_distance_positive (et_drawing_frame.paper_dimension (
									axis => AXIS_Y, paper_size => sheet_paper_size));
			
			-- calculate the new y position
			--new_y				:= sheet_height - distance_y (point_actual);
			--new_y				:= sheet_height - distance (axis => Y, point => point_actual);
			new_y				:= sheet_height - get_y (point_actual);

			-- assign the new y position to the given point
			--set_y (point_actual, new_y);
			set (point_actual, AXIS_Y, new_y);
		end move;


		
		
		-- Sets the layout_sheet_height depending on the paper size
		-- of the layout sheet.
		procedure prepare_layout_y_movements is
			use et_board_coordinates;
			use pac_geometry_2;
			use et_drawing_frame;
			
			-- The paper size of a board/layout drawing:
			board_paper_size : type_paper_size;
			
		begin -- prepare_layout_y_movements
			-- Fetch the paper size of the current layout design:
			board_paper_size := element (module_cursor).board.paper_size;
			
			log (text => "layout paper size " 
				 & to_string (board_paper_size), 
				 level => log_threshold + 2);
			
			-- Get the paper height of the sheet:

			--layout_sheet_height := et_pcb.frames.paper_dimension (
			--  axis => Y, paper_size => board_paper_size);

			layout_sheet_height := pac_geometry_2.type_distance_positive (
				et_drawing_frame.paper_dimension (axis => AXIS_Y, paper_size => board_paper_size));
		end prepare_layout_y_movements;


		
		-- Transposes the given point in layout from the kicad frame to the ET native frame.
		-- KiCad frames have the origin in the upper left corner.
		-- ET frames have the origin in the lower left corner.
		procedure move (point : in out et_board_coordinates.pac_geometry_2.type_vector_model) is
			use et_board_coordinates;
			use pac_geometry_2;
			new_y : type_position_axis;
		begin
			new_y := layout_sheet_height - get_y (point);
			set (point, AXIS_Y, new_y);
		end move;


		-- Transposes the given arc in layout from the kicad frame to the ET native frame.
		-- KiCad frames have the origin in the upper left corner.
		-- ET frames have the origin in the lower left corner.
		procedure move (
			arc : in out et_board_coordinates.pac_geometry_2.type_arc'class) 
		is 
			use et_board_coordinates;
			use pac_geometry_2;
			new_y : type_position_axis;
			S, E, C : type_vector_model;
		begin
			C := get_center (arc);
			new_y := layout_sheet_height - get_y (C);
			set (C, AXIS_Y, new_y);
			set_center (arc, C);
			
			S := get_A (arc);
			new_y := layout_sheet_height - get_y (S);
			set (S, AXIS_Y, new_y);
			set_A (arc, S);

			E := get_B (arc);
			new_y := layout_sheet_height - get_y (E);
			set (E, AXIS_Y, new_y);
			set_B (arc, E);
		end move;
		

		-- Transposes the given circle in layout from the kicad frame to the ET native frame.
		-- KiCad frames have the origin in the upper left corner.
		-- ET frames have the origin in the lower left corner.
		procedure move (
			circle : in out et_board_coordinates.pac_geometry_2.type_circle'class) 
		is 
			use et_board_coordinates;
			use pac_geometry_2;
			new_y : type_position_axis;
			C : type_vector_model;
		begin
			C := get_center (circle);
			new_y := layout_sheet_height - get_y (C);
			set (C, AXIS_Y, new_y);
			set_center (circle, C);
		end move;

		
		
		-- Changes the path and y position of text notes (in schematic):
		procedure flatten_notes (
			module_name	: in et_kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.pcb.type_module) 
		is
			use et_kicad.schematic.type_texts;
			note_cursor : et_kicad.schematic.type_texts.cursor := module.notes.first;

			
			procedure change_path (note : in out et_kicad.schematic.type_text) is
				use et_schematic_coordinates;				
				use et_kicad_coordinates;
			begin
				log (text => "note '" & et_text.to_string (note.content) & "'", level => log_threshold + 3);
				log_indentation_up;
				
				log (text => before & to_string (position => note.position, scope => SHEET),
					 level => log_threshold + 4);

				-- Move position from negative to positive y.
				move (note.position);

				log (text => now & to_string (position => note.position, scope => SHEET),
					 level => log_threshold + 4);

				log_indentation_down;
			end change_path;

			
		begin -- flatten_notes
			log (text => "text notes ...", level => log_threshold + 2);
			log_indentation_up;
			
			while note_cursor /= et_kicad.schematic.type_texts.no_element loop
				et_kicad.schematic.type_texts.update_element (
					container	=> module.notes,
					position	=> note_cursor,
					process		=> change_path'access);

				next (note_cursor);
			end loop;

			log_indentation_down;
		end flatten_notes;

		

		procedure flatten_frames (
			module_name	: in et_kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.pcb.type_module) is
		-- Changes the path of drawing frames (in schematic) to root path.
			
			use et_kicad.schematic.type_frames;
			frame_cursor : et_kicad.schematic.type_frames.cursor := module.frames.first;

			
			procedure change_path (frame : in out et_kicad.schematic.type_frame) is
				use et_schematic_coordinates;
			begin
				-- CS what should be logged here ?
				log_indentation_up;
				
				log (text => before & et_kicad_coordinates.to_string (position => frame.coordinates, scope => et_kicad_coordinates.MODULE),
					 level => log_threshold + 4);

				et_kicad_coordinates.set_path (frame.coordinates, root);

				log (text => now & et_kicad_coordinates.to_string (position => frame.coordinates, scope => et_kicad_coordinates.MODULE),
					level => log_threshold + 4);

				log_indentation_down;
			end change_path;

			
		begin -- flatten_frames
			log (text => "frames ...", level => log_threshold + 2);
			log_indentation_up;
			
			while frame_cursor /= et_kicad.schematic.type_frames.no_element loop
				et_kicad.schematic.type_frames.update_element (
					container	=> module.frames,
					position	=> frame_cursor,
					process		=> change_path'access);

				next (frame_cursor);
			end loop;

			log_indentation_down;
		end flatten_frames;


		
		-- Changes the path and y position of units of components (in schematic) to root path.
		-- Moves the y position of components (in layout).
		procedure flatten_components (
			module_name	: in et_kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.pcb.type_module) 
		is			
			use et_kicad.schematic.type_components_schematic;
			component_cursor : et_kicad.schematic.type_components_schematic.cursor := module.components.first;

			use et_device_name;
			
			
			procedure query_units (
				reference	: in type_device_name;
				component	: in out et_kicad.schematic.type_component_schematic) 
			is
				use et_schematic_coordinates;
				use et_kicad.schematic.type_units_schematic;
				unit_cursor : et_kicad.schematic.type_units_schematic.cursor := component.units.first;

				use pac_unit_name;
				
				
				procedure change_path (
					unit_name	: in pac_unit_name.bounded_string;
					unit		: in out et_kicad.schematic.type_unit_schematic) 
				is begin
					log (text => "unit " & to_string (unit_name), level => log_threshold + 4);
					log_indentation_up;
					
					log (text => before & et_kicad_coordinates.to_string (position => unit.position, scope => et_kicad_coordinates.MODULE),
						level => log_threshold + 4);

					et_kicad_coordinates.set_path (unit.position, root);

					move (unit.position); -- Move position from negative to positive y.

					log (text => now & et_kicad_coordinates.to_string (position => unit.position, scope => et_kicad_coordinates.MODULE),
						level => log_threshold + 4);

					log_indentation_down;
				end change_path;

				
				-- moves the position of the package in layout
				procedure move_package is
					use et_device_appearance;
					use et_board_coordinates;
					use et_board_coordinates.pac_geometry_2;
				begin
					if component.appearance = APPEARANCE_PCB then
						log_indentation_up;
						
						log (text => "package", level => log_threshold + 4);
						
						log_indentation_up;
						log (text => before & to_string (component.position.place), level => log_threshold + 4);
						move (point => component.position.place);
						log (text => now & to_string (component.position.place), level => log_threshold + 4);
						
						log_indentation_down;
						log_indentation_down;
					end if;
				end move_package;

				
			begin -- query_units
				log (text => to_string (key (component_cursor)), level => log_threshold + 3);
				log_indentation_up;

				while unit_cursor /= et_kicad.schematic.type_units_schematic.no_element loop
					
					et_kicad.schematic.type_units_schematic.update_element (
						container	=> component.units,
						position	=> unit_cursor,
						process		=> change_path'access);

					next (unit_cursor);
				end loop;

				-- If a board design has been imported, move y position of package in layout.
				if board_available then
					move_package;
				end if;
				
				log_indentation_down;
			end query_units;

			
		begin -- flatten_components
			log (text => "components ...", level => log_threshold + 2);
			log_indentation_up;
			
			while component_cursor /= et_kicad.schematic.type_components_schematic.no_element loop

				et_kicad.schematic.type_components_schematic.update_element (
					container	=> module.components,
					position	=> component_cursor,
					process		=> query_units'access);

				next (component_cursor);
			end loop;

			log_indentation_down;
		end flatten_components;
		

		procedure flatten_nets (
			module_name	: in et_kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.pcb.type_module) 
		is
		-- Changes the path and y position of net segments, junctions and labels (in schematic) to root path.
		-- MOves the y position of copper objects (in layout).

			use et_kicad.schematic.type_nets;
			net_cursor : et_kicad.schematic.type_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_kicad.schematic.type_net) is

				use et_kicad.schematic.type_strands;
				strand_cursor : et_kicad.schematic.type_strands.cursor := net.strands.first;

				procedure query_segments (strand : in out et_kicad.schematic.type_strand) is
					use et_schematic_coordinates.pac_geometry_2;
					use et_kicad.schematic.type_net_segments;
					segment_cursor : et_kicad.schematic.type_net_segments.cursor := strand.segments.first;

					procedure change_path_of_segment (segment : in out et_kicad.schematic.type_net_segment) is
						use et_schematic_coordinates;
						
						use et_kicad.schematic.type_simple_labels;
						simple_label_cursor : et_kicad.schematic.type_simple_labels.cursor := segment.label_list_simple.first;

						
						procedure move_simple_label (label : in out et_kicad.schematic.type_net_label_simple) is
						-- Moves the given simple label from kicad frame to native frame.
						begin
							log (text => "simple label " & before & to_string (label.coordinates), level => log_threshold + 3);
							
							-- As supportive point that provides the sheet number we pass the segment start position.
							-- coordinates of net labels do not posess a sheet number.
							move (point_actual => label.coordinates, point_help => segment.coordinates_start);

							log (text => "simple label " & now & to_string (label.coordinates), level => log_threshold + 3);							
						end move_simple_label;

						
						use et_kicad.schematic.type_tag_labels;
						tag_label_cursor : et_kicad.schematic.type_tag_labels.cursor := segment.label_list_tag.first;

						
						procedure move_tag_label (label : in out et_kicad.schematic.type_net_label_tag) is
						-- Moves the given tag label from kicad frame to native frame.
						begin
							log (text => "tag label " & before & to_string (label.coordinates), level => log_threshold + 3);
							
							-- As supportive point that provides the sheet number we pass the segment start position.
							-- coordinates of net labels do not posess a sheet number.
							move (point_actual => label.coordinates, point_help => segment.coordinates_start);
							
							log (text => "tag label " & now & to_string (label.coordinates), level => log_threshold + 3);
						end move_tag_label;

						
						use et_kicad.schematic.type_junctions;
						junction_cursor : et_kicad.schematic.type_junctions.cursor := segment.junctions.first;

						
						procedure change_path_of_junction (junction : in out et_kicad.schematic.type_net_junction) is
						-- Moves the given net junction from kicad frame to native frame.
							use et_kicad_coordinates;
						begin
							log (text => "junction " & before & et_kicad_coordinates.to_string (
								position => junction.coordinates, scope => et_kicad_coordinates.MODULE),
								level => log_threshold + 3);

							set_path (junction.coordinates, root);
							move (junction.coordinates);

							log (text => "junction " & now & to_string (
								position => junction.coordinates, scope => et_kicad_coordinates.MODULE),
								level => log_threshold + 3);
								 
						end change_path_of_junction;

						
					begin -- change_path_of_segment
						log (text => "schematic net segment", level => log_threshold + 3);
						log_indentation_up;

						-- start point of net segment
						log (text => "start " & before & et_kicad_coordinates.to_string (
							position => segment.coordinates_start, scope => et_kicad_coordinates.MODULE),
							level => log_threshold + 3);

						et_kicad_coordinates.set_path (segment.coordinates_start, root);

						move (segment.coordinates_start); -- Move position from negative to positive y.
						
						log (text => "start " & now & et_kicad_coordinates.to_string (
							position => segment.coordinates_start, scope => et_kicad_coordinates.MODULE),
							level => log_threshold + 3);

						-- end point of net segment
						log (text => "end   " & before & et_kicad_coordinates.to_string (
							position => segment.coordinates_end, scope => et_kicad_coordinates.MODULE),
							level => log_threshold + 3);

						et_kicad_coordinates.set_path (segment.coordinates_end, root);

						move (segment.coordinates_end); -- Move position from negative to positive y.
						
						log (text => "end   " & now & et_kicad_coordinates.to_string (
							position => segment.coordinates_end, scope => et_kicad_coordinates.MODULE),
							level => log_threshold + 3);

						-- Move y of simple net labels.
						while simple_label_cursor /= et_kicad.schematic.type_simple_labels.no_element loop
							et_kicad.schematic.type_simple_labels.update_element (
								container	=> segment.label_list_simple,
								position	=> simple_label_cursor,
								process 	=> move_simple_label'access);
							next (simple_label_cursor);
						end loop;

						-- Move y of tag net labels.
						while tag_label_cursor /= et_kicad.schematic.type_tag_labels.no_element loop
							et_kicad.schematic.type_tag_labels.update_element (
								container	=> segment.label_list_tag,
								position	=> tag_label_cursor,
								process 	=> move_tag_label'access);
							next (tag_label_cursor);
						end loop;

						-- Change path of junctions (incl. moving y):
						while junction_cursor /= et_kicad.schematic.type_junctions.no_element loop
							et_kicad.schematic.type_junctions.update_element (
								container	=> segment.junctions,
								position	=> junction_cursor,
								process 	=> change_path_of_junction'access);
							next (junction_cursor);
						end loop;
						
						log_indentation_down;

					end change_path_of_segment;

					
				begin -- query_segments

					-- Move the start coordinates of the strand from kicad frame to native frame:
					log (text => "schematic strand start " & before 
						 & to_string (et_kicad_coordinates.get_point (strand.position)), 
						 level => log_threshold + 3);
					
					move (strand.position); 
					log (text => "schematic strand start " & now 
						 & to_string (et_kicad_coordinates.get_point (strand.position)), 
						 level => log_threshold + 3);

					-- Change path of segments:
					while segment_cursor /= et_kicad.schematic.type_net_segments.no_element loop

						et_kicad.schematic.type_net_segments.update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> change_path_of_segment'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;

				
				procedure move_route is
				-- Move y position of copper objects of the net: lines, arcs, vias, polygons

					use pac_conductor_lines;
					use pac_conductor_arcs;
					
					use et_vias;
					use pac_vias;

					use et_fill_zones.boards;
					use pac_route_solid;
					
					line_cursor : pac_conductor_lines.cursor := net.route.lines.first;
					arc_cursor	: pac_conductor_arcs.cursor := net.route.arcs.first;
					via_cursor	: pac_vias.cursor := net.route.vias.first;
					poly_cursor	: pac_route_solid.cursor := net.route.zones.solid.first;

					board_track : constant string (1..12) := "board track ";
					
					
					procedure move_line (line : in out type_conductor_line) is 
						p_scratch : et_board_coordinates.pac_geometry_2.type_vector_model;
					begin
						log (text => board_track & "line", level => log_threshold + 4);
						log_indentation_up;

						log (text => before & to_string (line, true), -- log incl. width
							 level => log_threshold + 4);
						
						-- move (line.A);
						-- move (line.B);

						p_scratch := get_A (line);
						move (p_scratch);
						set_A (line, p_scratch);

						p_scratch := get_B (line);
						move (p_scratch);
						set_B (line, p_scratch);
						
						log (text => now & to_string (line, true),  -- log incl. width
							 level => log_threshold + 4);
						
						log_indentation_down;
					end move_line;

					
					procedure move_arc (arc : in out type_conductor_arc) is begin
						log (text => board_track & "arc", level => log_threshold + 4);
						log_indentation_up;

						log (text => before & to_string (arc), level => log_threshold + 4);
						move (arc);
						log (text => now & to_string (arc), level => log_threshold + 4);
						
						log_indentation_down;
					end move_arc;

					
					procedure move_via (via : in out type_via) is
						use et_board_coordinates.pac_geometry_2;
					begin
						log (text => board_track & "via", level => log_threshold + 4);
						log_indentation_up;

						log (text => before & to_string (via.position), level => log_threshold + 4);

						move (via.position);

						log (text => now & to_string (via.position), level => log_threshold + 4);
						
						log_indentation_down;
					end move_via;

					
					procedure move_polygon (polygon : in out type_route_solid) is begin
						log (text => "polygon segments", level => log_threshold + 4);
						et_board_coordinates.pac_contours.transpose_contour (polygon, layout_sheet_height);
					end move_polygon;

					
				begin -- move_route
					
					-- Move lines:
					while line_cursor /= pac_conductor_lines.no_element loop
						pac_conductor_lines.update_element (
							container 	=> net.route.lines,
							position	=> line_cursor,
							process		=> move_line'access);
						
						next (line_cursor);
					end loop;

					-- Move arcs:
					while arc_cursor /= pac_conductor_arcs.no_element loop
						pac_conductor_arcs.update_element (
							container 	=> net.route.arcs,
							position	=> arc_cursor,
							process		=> move_arc'access);

						next (arc_cursor);
					end loop;

					-- Move vias:
					while via_cursor /= pac_vias.no_element loop
						pac_vias.update_element (
							container 	=> net.route.vias,
							position	=> via_cursor,
							process		=> move_via'access);

						next (via_cursor);
					end loop;

					while poly_cursor /= pac_route_solid.no_element loop
						pac_route_solid.update_element (
							container 	=> net.route.zones.solid,
							position	=> poly_cursor,
							process		=> move_polygon'access);

						next (poly_cursor);
					end loop;
					
				end move_route;

				
			begin -- query_strands

				-- schematic related:
				while strand_cursor /= et_kicad.schematic.type_strands.no_element loop

					et_kicad.schematic.type_strands.update_element (
						container	=> net.strands,
						position	=> strand_cursor,
						process		=> query_segments'access);
				
					next (strand_cursor);
				end loop;

				-- layout related:
				-- Copper objects of the net: lines, arcs, vias, polygons
				if board_available then
					move_route;
				end if;

			end query_strands;

			
		begin -- flatten_nets
			log (text => "nets ...", level => log_threshold + 2);
			log_indentation_up;
			
			while net_cursor /= et_kicad.schematic.type_nets.no_element loop
				log (text => to_string (key (net_cursor)), level => log_threshold + 3);

				log_indentation_up;
				
				et_kicad.schematic.type_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);

				log_indentation_down;
				
				next (net_cursor);
			end loop;

			log_indentation_down;
		end flatten_nets;


		
		-- Moves y positon of general (non-component related) layout objects from kicad frame to native frame.
		procedure move_general_board_stuff (
			module_name	: in et_kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.pcb.type_module) 
		is
			log_threshold_add : type_log_level := 2;
			
			procedure move_silk_screen is
				use pac_silk_lines;
				
				lines_cursor : pac_silk_lines.cursor;

				use pac_silk_arcs;
				arcs_cursor : pac_silk_arcs.cursor;

				use pac_silk_circles;
				circles_cursor : pac_silk_circles.cursor;

				use pac_silk_zones;
				polygons_cursor : pac_silk_zones.cursor;

				use pac_silk_texts;
				texts_cursor : pac_silk_texts.cursor;
				
				board_silk_screen : constant string := "board silk screen ";

				
				procedure move_line (line : in out type_silk_line) is 
					p_scratch : et_board_coordinates.pac_geometry_2.type_vector_model;
				begin
					log (text => board_silk_screen & "line", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (line), level => log_threshold + log_threshold_add);

					-- move (line.A);
					-- move (line.B);

					p_scratch := get_A (line);
					move (p_scratch);
					set_A (line, p_scratch);

					p_scratch := get_B (line);
					move (p_scratch);
					set_B (line, p_scratch);
					
					log (text => now & to_string (line), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				
				procedure move_arc (arc : in out type_silk_arc) is begin
					log (text => board_silk_screen & "arc", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (arc), level => log_threshold + log_threshold_add);
					move (arc);
					log (text => now & to_string (arc), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				
				procedure move_circle (circle : in out type_silk_circle) is 
					use et_board_coordinates.pac_geometry_2;
				begin
					log (text => board_silk_screen & "circle", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & " center" & to_string (circle), level => log_threshold + log_threshold_add);
					move (circle);
					log (text => now & " center" & to_string (circle), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

				
				procedure move_polygon (polygon : in out type_silk_zone) is begin
					log (text => board_silk_screen & "zone segments", level => log_threshold + log_threshold_add);
					et_board_coordinates.pac_contours.transpose_contour (polygon, layout_sheet_height);
				end;

				
				procedure move_text (text : in out type_silk_text) is
					use et_board_coordinates.pac_geometry_2;
				begin
					log (text => board_silk_screen & "text", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (text.position), level => log_threshold + log_threshold_add);

					move (text.position.place);
					
					log (text => now & to_string (text.position), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_text;

				
			begin -- move_silk_screen
				
				-- LINES TOP
				lines_cursor := module.board.silkscreen.top.lines.first;
				while lines_cursor /= pac_silk_lines.no_element loop
					pac_silk_lines.update_element (
						container	=> module.board.silkscreen.top.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				
				-- LINES BOTTOM
				lines_cursor := module.board.silkscreen.bottom.lines.first;
				while lines_cursor /= pac_silk_lines.no_element loop
					pac_silk_lines.update_element (
						container	=> module.board.silkscreen.bottom.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				
				-- ARCS TOP
				arcs_cursor := module.board.silkscreen.top.arcs.first;
				while arcs_cursor /= pac_silk_arcs.no_element loop
					pac_silk_arcs.update_element (
						container	=> module.board.silkscreen.top.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				
				-- ARCS BOTTOM
				arcs_cursor := module.board.silkscreen.bottom.arcs.first;
				while arcs_cursor /= pac_silk_arcs.no_element loop
					pac_silk_arcs.update_element (
						container	=> module.board.silkscreen.bottom.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;			

				
				-- CIRCLES TOP
				circles_cursor := module.board.silkscreen.top.circles.first;
				while circles_cursor /= pac_silk_circles.no_element loop
					pac_silk_circles.update_element (
						container	=> module.board.silkscreen.top.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				
				-- CIRCLES BOTTOM
				circles_cursor := module.board.silkscreen.bottom.circles.first;
				while circles_cursor /= pac_silk_circles.no_element loop
					pac_silk_circles.update_element (
						container	=> module.board.silkscreen.bottom.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				
				-- POLYGONS TOP
				polygons_cursor := module.board.silkscreen.top.zones.first;
				while polygons_cursor /= pac_silk_zones.no_element loop
					pac_silk_zones.update_element (
						container	=> module.board.silkscreen.top.zones,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				
				-- POLYGONS BOTTOM
				polygons_cursor := module.board.silkscreen.bottom.zones.first;
				while polygons_cursor /= pac_silk_zones.no_element loop
					pac_silk_zones.update_element (
						container	=> module.board.silkscreen.bottom.zones,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;	

				
				-- TEXTS TOP
				texts_cursor := module.board.silkscreen.top.texts.first;
				while texts_cursor /= pac_silk_texts.no_element loop
					pac_silk_texts.update_element (
						container	=> module.board.silkscreen.top.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;

				
				-- TEXTS BOTTOM
				texts_cursor := module.board.silkscreen.bottom.texts.first;
				while texts_cursor /= pac_silk_texts.no_element loop
					pac_silk_texts.update_element (
						container	=> module.board.silkscreen.bottom.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;

			end move_silk_screen;

			
			procedure move_assembly_documentation is
				use et_assy_doc;
				use pac_doc_lines;
				
				lines_cursor : pac_doc_lines.cursor;

				use pac_doc_arcs;
				arcs_cursor : pac_doc_arcs.cursor;

				use pac_doc_circles;
				circles_cursor : pac_doc_circles.cursor;

				use pac_doc_zones;
				polygons_cursor : pac_doc_zones.cursor;

				use pac_doc_texts;
				texts_cursor : pac_doc_texts.cursor;
				
				doc : constant string := "board assembly documentation ";

				
				procedure move_line (line : in out type_doc_line) is 
					p_scratch : et_board_coordinates.pac_geometry_2.type_vector_model;
				begin
					log (text => doc & "line", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (line), level => log_threshold + log_threshold_add);

					-- move (line.A);
					-- move (line.B);
					
					p_scratch := get_A (line);
					move (p_scratch);
					set_A (line, p_scratch);

					p_scratch := get_B (line);
					move (p_scratch);
					set_B (line, p_scratch);
					
					log (text => now & to_string (line), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				
				procedure move_arc (arc : in out type_doc_arc) is begin
					log (text => doc & "arc", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (arc), level => log_threshold + log_threshold_add);

					move (arc);
					
					log (text => now & to_string (arc), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				
				procedure move_circle (circle : in out type_doc_circle) is 
					use et_board_coordinates.pac_geometry_2;
				begin
					log (text => doc & "circle", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & " center" & to_string (circle), level => log_threshold + log_threshold_add);
					move (circle);
					log (text => now & " center" & to_string (circle), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

				
				procedure move_polygon (polygon : in out type_doc_zone) is begin
					log (text => doc & "polygon segments", level => log_threshold + log_threshold_add);
					et_board_coordinates.pac_contours.transpose_contour (polygon, layout_sheet_height);
				end;

				
				procedure move_text (text : in out type_doc_text) is
					use et_board_coordinates;
					use et_board_coordinates.pac_geometry_2;
				begin
					log (text => doc & "text", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (text.position), level => log_threshold + log_threshold_add);

					move (text.position.place);
					
					log (text => now & to_string (text.position), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_text;

				
			begin -- move_assembly_documentation
				
				-- LINES TOP
				lines_cursor := module.board.assy_doc.top.lines.first;
				while lines_cursor /= pac_doc_lines.no_element loop
					pac_doc_lines.update_element (
						container	=> module.board.assy_doc.top.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				
				-- LINES BOTTOM
				lines_cursor := module.board.assy_doc.bottom.lines.first;
				while lines_cursor /= pac_doc_lines.no_element loop
					pac_doc_lines.update_element (
						container	=> module.board.assy_doc.bottom.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				
				-- ARCS TOP
				arcs_cursor := module.board.assy_doc.top.arcs.first;
				while arcs_cursor /= pac_doc_arcs.no_element loop
					pac_doc_arcs.update_element (
						container	=> module.board.assy_doc.top.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				
				-- ARCS BOTTOM
				arcs_cursor := module.board.assy_doc.bottom.arcs.first;
				while arcs_cursor /= pac_doc_arcs.no_element loop
					pac_doc_arcs.update_element (
						container	=> module.board.assy_doc.bottom.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;			

				
				-- CIRCLES TOP
				circles_cursor := module.board.assy_doc.top.circles.first;
				while circles_cursor /= pac_doc_circles.no_element loop
					pac_doc_circles.update_element (
						container	=> module.board.assy_doc.top.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				
				-- CIRCLES BOTTOM
				circles_cursor := module.board.assy_doc.bottom.circles.first;
				while circles_cursor /= pac_doc_circles.no_element loop
					pac_doc_circles.update_element (
						container	=> module.board.assy_doc.bottom.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				
				-- POLYGONS TOP
				polygons_cursor := module.board.assy_doc.top.zones.first;
				while polygons_cursor /= pac_doc_zones.no_element loop
					pac_doc_zones.update_element (
						container	=> module.board.assy_doc.top.zones,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				
				-- POLYGONS BOTTOM
				polygons_cursor := module.board.assy_doc.bottom.zones.first;
				while polygons_cursor /= pac_doc_zones.no_element loop
					pac_doc_zones.update_element (
						container	=> module.board.assy_doc.bottom.zones,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;	

				
				-- TEXTS TOP
				texts_cursor := module.board.assy_doc.top.texts.first;
				while texts_cursor /= pac_doc_texts.no_element loop
					pac_doc_texts.update_element (
						container	=> module.board.assy_doc.top.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;

				
				-- TEXTS BOTTOM
				texts_cursor := module.board.assy_doc.bottom.texts.first;
				while texts_cursor /= pac_doc_texts.no_element loop
					pac_doc_texts.update_element (
						container	=> module.board.assy_doc.bottom.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;
				
			end move_assembly_documentation;

			
			
			procedure move_stencil is
				use et_stencil;
				use pac_stencil_lines;
				lines_cursor : pac_stencil_lines.cursor;

				use pac_stencil_arcs;
				arcs_cursor : pac_stencil_arcs.cursor;

				use pac_stencil_circles;
				circles_cursor : pac_stencil_circles.cursor;

				use pac_stencil_zones;
				polygons_cursor : pac_stencil_zones.cursor;

				stencil : constant string := "board stencil ";
				
				procedure move_line (line : in out type_stencil_line) is
					use et_pcb;
					p_scratch : et_board_coordinates.pac_geometry_2.type_vector_model;
				begin
					log (text => stencil & "line", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (line), level => log_threshold + log_threshold_add);
     
					-- move (line.A);
					-- move (line.B);

					p_scratch := get_A (line);
					move (p_scratch);
					set_A (line, p_scratch);
     
					p_scratch := get_B (line);
					move (p_scratch);
					set_B (line, p_scratch);
					
					log (text => now & to_string (line), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				
				procedure move_arc (arc : in out type_stencil_arc) is
					use et_pcb;
				begin
					log (text => stencil & "arc", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (arc), level => log_threshold + log_threshold_add);
					move (arc);					
					log (text => now & to_string (arc), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				
				procedure move_circle (circle : in out type_stencil_circle) is
					use et_board_coordinates.pac_geometry_2;
				begin
					log (text => stencil & "circle", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & " center" & to_string (circle), level => log_threshold + log_threshold_add);
					move (circle);					
					log (text => now & " center" & to_string (circle), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;


				procedure move_polygon (polygon : in out type_stencil_zone) is begin
					log (text => stencil & "zone corner points", level => log_threshold + log_threshold_add);
					move_polygon (polygon);
				end move_polygon;

				
			begin -- move_stencil
				
				-- LINES TOP
				lines_cursor := module.board.stencil.top.lines.first;
				while lines_cursor /= pac_stencil_lines.no_element loop
					pac_stencil_lines.update_element (
						container	=> module.board.stencil.top.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				
				-- LINES BOTTOM
				lines_cursor := module.board.stencil.bottom.lines.first;
				while lines_cursor /= pac_stencil_lines.no_element loop
					pac_stencil_lines.update_element (
						container	=> module.board.stencil.bottom.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				
				-- ARCS TOP
				arcs_cursor := module.board.stencil.top.arcs.first;
				while arcs_cursor /= pac_stencil_arcs.no_element loop
					pac_stencil_arcs.update_element (
						container	=> module.board.stencil.top.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				
				-- ARCS BOTTOM
				arcs_cursor := module.board.stencil.bottom.arcs.first;
				while arcs_cursor /= pac_stencil_arcs.no_element loop
					pac_stencil_arcs.update_element (
						container	=> module.board.stencil.bottom.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;			

				
				-- CIRCLES TOP
				circles_cursor := module.board.stencil.top.circles.first;
				while circles_cursor /= pac_stencil_circles.no_element loop
					pac_stencil_circles.update_element (
						container	=> module.board.stencil.top.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				
				-- CIRCLES BOTTOM
				circles_cursor := module.board.stencil.bottom.circles.first;
				while circles_cursor /= pac_stencil_circles.no_element loop
					pac_stencil_circles.update_element (
						container	=> module.board.stencil.bottom.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				
				-- ZONES TOP
				polygons_cursor := module.board.stencil.top.zones.first;
				while polygons_cursor /= pac_stencil_zones.no_element loop
					pac_stencil_zones.update_element (
						container	=> module.board.stencil.top.zones,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				
				-- ZONES BOTTOM
				polygons_cursor := module.board.stencil.bottom.zones.first;
				while polygons_cursor /= pac_stencil_zones.no_element loop
					pac_stencil_zones.update_element (
						container	=> module.board.stencil.bottom.zones,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;	
		
			end move_stencil;


			
			procedure move_stop_mask is
				use et_stopmask;
				use pac_stop_lines;
				lines_cursor : pac_stop_lines.cursor;

				use pac_stop_arcs;
				arcs_cursor : pac_stop_arcs.cursor;

				use pac_stop_circles;
				circles_cursor : pac_stop_circles.cursor;

				use pac_stop_zones;
				polygons_cursor : pac_stop_zones.cursor;

				use pac_stop_texts;
				texts_cursor : pac_stop_texts.cursor;
				
				stop : constant string := "board stop mask ";
				
				procedure move_line (line : in out type_stop_line) is
					use et_pcb;
					p_scratch : et_board_coordinates.pac_geometry_2.type_vector_model;	
				begin
					log (text => stop & "line", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (line), level => log_threshold + log_threshold_add);

					-- move (line.A);
					-- move (line.B);

					p_scratch := get_A (line);
					move (p_scratch);
					set_A (line, p_scratch);
     
					p_scratch := get_B (line);
					move (p_scratch);
					set_B (line, p_scratch);
					
					log (text => now & to_string (line), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				
				procedure move_arc (arc : in out type_stop_arc) is
					use et_pcb;
				begin
					log (text => stop & "arc", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (arc), level => log_threshold + log_threshold_add);

					move (arc);
					
					log (text => now & to_string (arc), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				
				procedure move_circle (circle : in out type_stop_circle) is
					use et_board_coordinates.pac_geometry_2;
				begin
					log (text => stop & "circle", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & " center" & to_string (circle), level => log_threshold + log_threshold_add);
					move (circle);
					log (text => now & " center" & to_string (circle), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

				
				procedure move_polygon (polygon : in out type_stop_zone) is begin
					log (text => stop & "zone corner points", level => log_threshold + log_threshold_add);
					et_board_coordinates.pac_contours.transpose_contour (polygon, layout_sheet_height);
				end move_polygon;

				
				procedure move_text (text : in out type_stop_text) is
					use et_board_coordinates;
					use et_board_coordinates.pac_geometry_2;
				begin
					log (text => stop & "text", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (text.position), level => log_threshold + log_threshold_add);

					move (text.position.place);
					
					log (text => now & to_string (text.position), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_text;

				
			begin -- move_stop_mask
				
				-- LINES TOP
				lines_cursor := module.board.stopmask.top.lines.first;
				while lines_cursor /= pac_stop_lines.no_element loop
					pac_stop_lines.update_element (
						container	=> module.board.stopmask.top.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				
				-- LINES BOTTOM
				lines_cursor := module.board.stopmask.bottom.lines.first;
				while lines_cursor /= pac_stop_lines.no_element loop
					pac_stop_lines.update_element (
						container	=> module.board.stopmask.bottom.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				
				-- ARCS TOP
				arcs_cursor := module.board.stopmask.top.arcs.first;
				while arcs_cursor /= pac_stop_arcs.no_element loop
					pac_stop_arcs.update_element (
						container	=> module.board.stopmask.top.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				
				-- ARCS BOTTOM
				arcs_cursor := module.board.stopmask.bottom.arcs.first;
				while arcs_cursor /= pac_stop_arcs.no_element loop
					pac_stop_arcs.update_element (
						container	=> module.board.stopmask.bottom.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;			

				
				-- CIRCLES TOP
				circles_cursor := module.board.stopmask.top.circles.first;
				while circles_cursor /= pac_stop_circles.no_element loop
					pac_stop_circles.update_element (
						container	=> module.board.stopmask.top.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				
				-- CIRCLES BOTTOM
				circles_cursor := module.board.stopmask.bottom.circles.first;
				while circles_cursor /= pac_stop_circles.no_element loop
					pac_stop_circles.update_element (
						container	=> module.board.stopmask.bottom.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				
				-- ZONES TOP
				polygons_cursor := module.board.stopmask.top.zones.first;
				while polygons_cursor /= pac_stop_zones.no_element loop
					pac_stop_zones.update_element (
						container	=> module.board.stopmask.top.zones,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				
				-- ZONES BOTTOM
				polygons_cursor := module.board.stopmask.bottom.zones.first;
				while polygons_cursor /= pac_stop_zones.no_element loop
					pac_stop_zones.update_element (
						container	=> module.board.stopmask.bottom.zones,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;	

				
				-- TEXTS TOP
				texts_cursor := module.board.stopmask.top.texts.first;
				while texts_cursor /= pac_stop_texts.no_element loop
					pac_stop_texts.update_element (
						container	=> module.board.stopmask.top.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;

				
				-- TEXTS BOTTOM
				texts_cursor := module.board.stopmask.bottom.texts.first;
				while texts_cursor /= pac_stop_texts.no_element loop
					pac_stop_texts.update_element (
						container	=> module.board.stopmask.bottom.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;
				
			end move_stop_mask;


			procedure move_keepout is
				use pac_keepout_zones;
				polygons_cursor : pac_keepout_zones.cursor;

				keepout : constant string := "board keepout ";
							
				procedure move_polygon (polygon : in out type_keepout_zone) is begin
					log (text => keepout & "zone segments", level => log_threshold + log_threshold_add);
					et_board_coordinates.pac_contours.transpose_contour (polygon, layout_sheet_height);
				end move_polygon;

				
			begin -- move_keepout
				
				-- POLYGONS TOP
				polygons_cursor := module.board.keepout.top.zones.first;
				while polygons_cursor /= pac_keepout_zones.no_element loop
					pac_keepout_zones.update_element (
						container	=> module.board.keepout.top.zones,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				
				-- POLYGONS BOTTOM
				polygons_cursor := module.board.keepout.bottom.zones.first;
				while polygons_cursor /= pac_keepout_zones.no_element loop
					pac_keepout_zones.update_element (
						container	=> module.board.keepout.bottom.zones,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;	
		
			end move_keepout;

			
			procedure move_contour is
				use et_board_coordinates;
				use pac_geometry_brd;
				use pac_geometry_2;
				use pac_contours;
				use pac_segments;
				
				contour : constant string := "board contour ";
				
				procedure move_line (s : in out type_segment) is 
					p_scratch : et_board_coordinates.pac_geometry_2.type_vector_model;
				begin
					log (text => contour & "line", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (s.segment_line), level => log_threshold + log_threshold_add);

					-- move (s.segment_line.A);
					-- move (s.segment_line.B);

					p_scratch := get_A (s.segment_line);
					move (p_scratch);
					set_A (s.segment_line, p_scratch);
     
					p_scratch := get_B (s.segment_line);
					move (p_scratch);
					set_B (s.segment_line, p_scratch);
					
					log (text => now & to_string (s.segment_line), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				
				procedure move_arc (s : in out type_segment) is begin
					log (text => contour & "arc", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (s.segment_arc), level => log_threshold + log_threshold_add);

					move (s.segment_arc);
					
					log (text => now & to_string (s.segment_arc), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				
				procedure move_segment (c : in pac_segments.cursor) is begin
					case element (c).shape is
						
						when LINE => 
							update_element (
								container	=> module.board.board_contour.outline.contour.segments,
								position	=> c,
								process		=> move_line'access);
							
						when ARC =>
							update_element (
								container	=> module.board.board_contour.outline.contour.segments,
								position	=> c,
								process		=> move_arc'access);

					end case;
				end move_segment;

				
				procedure move_outline is begin
					if module.board.board_contour.outline.contour.circular then

						-- move the single circle the outline consists of:
						log (text => contour & "circle", level => log_threshold + log_threshold_add);
						log_indentation_up;

						log (text => before & " center" 
							& to_string (module.board.board_contour.outline.contour.circle), 
							level => log_threshold + log_threshold_add);

						move (module.board.board_contour.outline.contour.circle);
						
						log (text => now & " center" 
							& to_string (module.board.board_contour.outline.contour.circle), 
							level => log_threshold + log_threshold_add);
								
						log_indentation_down;
					else
						-- move the segments of the outline:
						iterate (module.board.board_contour.outline.contour.segments, move_segment'access);
					end if;
				end move_outline;

				
			begin -- move_contour
				move_outline;

				-- CS:
				-- kicad does not distinguish between pcb outline and holes.
				-- So the holes should be extracted here from the outline segments.
				-- Holes are completely inside the board area and are thus detectable.
				
			end move_contour;


			
			procedure move_copper is
				use pac_conductor_lines;
				lines_cursor : pac_conductor_lines.cursor;

				use pac_conductor_arcs;
				arcs_cursor : pac_conductor_arcs.cursor;

				use pac_conductor_circles;
				circles_cursor : pac_conductor_circles.cursor;

				use et_fill_zones.boards;
				use pac_floating_solid;
				polygons_solid_cursor : pac_floating_solid.cursor;

				use pac_floating_hatched;
				polygons_hatched_cursor : pac_floating_hatched.cursor;

				use et_conductor_text.boards;
				use pac_conductor_texts;
				texts_cursor : pac_conductor_texts.cursor;

				use et_pcb_placeholders;
				use pac_text_placeholders_conductors;
				placeholders_cursor : pac_text_placeholders_conductors.cursor;
				
				board_copper : constant string := "board copper ";

				
				procedure move_line (line : in out type_conductor_line) is 
					p_scratch : et_board_coordinates.pac_geometry_2.type_vector_model;
				begin
					log (text => board_copper & "line", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (line, true),  -- log incl. width
						 level => log_threshold + log_threshold_add);

					-- move (line.A);
					-- move (line.B);

					p_scratch := get_A (line);
					move (p_scratch);
					set_A (line, p_scratch);
     
					p_scratch := get_B (line);
					move (p_scratch);
					set_B (line, p_scratch);
					
					log (text => now & to_string (line, true),  -- log incl. width
						 level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;


				procedure move_arc (arc : in out type_conductor_arc) is begin
					log (text => board_copper & "arc", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (arc), level => log_threshold + log_threshold_add);

					move (arc);
					
					log (text => now & to_string (arc), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				
				procedure move_circle (circle : in out type_conductor_circle) is
					use et_board_coordinates.pac_geometry_2;
				begin
					log (text => board_copper & "circle", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & " center" & to_string (circle), level => log_threshold + log_threshold_add);
					move (circle);					
					log (text => now & " center" & to_string (circle), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

				
				procedure move_polygon (polygon : in out type_floating_solid) is begin
					log (text => board_copper & "solid polygon segments", level => log_threshold + log_threshold_add);
					et_board_coordinates.pac_contours.transpose_contour (polygon, layout_sheet_height);
				end move_polygon;

				
				procedure move_polygon (polygon : in out type_floating_hatched) is begin
					log (text => board_copper & "hatched polygon segments", level => log_threshold + log_threshold_add);
					et_board_coordinates.pac_contours.transpose_contour (polygon, layout_sheet_height);
				end move_polygon;

				
				procedure move_text (text : in out et_conductor_text.boards.type_conductor_text) is
					use et_board_coordinates;
					use et_board_coordinates.pac_geometry_2;
				begin
					log (text => board_copper & "text", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (text.position), level => log_threshold + log_threshold_add);

					move (text.position.place);
					
					log (text => now & to_string (text.position), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_text;

				
				procedure move_placeholder (text : in out type_text_placeholder_conductors) is
					use et_board_coordinates;
					use et_board_coordinates.pac_geometry_2;
				begin
					log (text => board_copper & "text placeholder", level => log_threshold + log_threshold_add);
					log_indentation_up;

					log (text => before & to_string (text.position), level => log_threshold + log_threshold_add);

					move (text.position.place);
					
					log (text => now & to_string (text.position), level => log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_placeholder;

				
			begin -- move_copper
				
				-- LINES
				lines_cursor := module.board.conductors_floating.lines.first;
				while lines_cursor /= pac_conductor_lines.no_element loop
					pac_conductor_lines.update_element (
						container	=> module.board.conductors_floating.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				
				-- ARCS
				arcs_cursor := module.board.conductors_floating.arcs.first;
				while arcs_cursor /= pac_conductor_arcs.no_element loop
					pac_conductor_arcs.update_element (
						container	=> module.board.conductors_floating.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				
				-- CIRCLES
				circles_cursor := module.board.conductors_floating.circles.first;
				while circles_cursor /= pac_conductor_circles.no_element loop
					pac_conductor_circles.update_element (
						container	=> module.board.conductors_floating.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				
				-- FLOATING FILL ZONES
				-- solid
				polygons_solid_cursor := module.board.conductors_floating.zones.solid.first;
				while polygons_solid_cursor /= pac_floating_solid.no_element loop
					pac_floating_solid.update_element (
						container	=> module.board.conductors_floating.zones.solid,
						position	=> polygons_solid_cursor,
						process		=> move_polygon'access);
					
					next (polygons_solid_cursor);
				end loop;

				
				-- hatched
				polygons_hatched_cursor := module.board.conductors_floating.zones.hatched.first;
				while polygons_hatched_cursor /= pac_floating_hatched.no_element loop
					pac_floating_hatched.update_element (
						container	=> module.board.conductors_floating.zones.hatched,
						position	=> polygons_hatched_cursor,
						process		=> move_polygon'access);
					
					next (polygons_hatched_cursor);
				end loop;
				

				-- TEXTS
				texts_cursor := module.board.conductors_floating.texts.first;
				while texts_cursor /= pac_conductor_texts.no_element loop
					update_element (
						container	=> module.board.conductors_floating.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;

				
				-- TEXT PLACEHOLDERS
				placeholders_cursor := module.board.conductors_floating.placeholders.first;
				while placeholders_cursor /= pac_text_placeholders_conductors.no_element loop
					pac_text_placeholders_conductors.update_element (
						container	=> module.board.conductors_floating.placeholders,
						position	=> placeholders_cursor,
						process		=> move_placeholder'access);

					next (texts_cursor);
				end loop;
				
			end move_copper;

			
		begin -- move_general_board_stuff
			move_silk_screen;
			move_assembly_documentation;
			move_stencil;
			move_stop_mask;
			move_keepout;
			move_contour;
			move_copper; -- non-electric copper stuff !!! (like freetracks)
		end move_general_board_stuff;


		
		procedure flatten_netlist (
		-- Changes the path and y position of ports.
		-- NOTE: The netlist contains nets with their connected ports.
			module_name	: in et_kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.pcb.type_module) 
		is
			use et_kicad.schematic.type_netlist;
			net_cursor : et_kicad.schematic.type_netlist.cursor := module.netlist.first;

			
			procedure query_ports (
				net_name	: in pac_net_name.bounded_string;
				ports		: in out et_kicad.schematic.pac_ports_with_reference.set) 
			is
				use et_kicad.schematic.pac_ports_with_reference;
				port_cursor : et_kicad.schematic.pac_ports_with_reference.cursor := ports.first;
				port : et_kicad.schematic.type_port_with_reference;
				
				use et_schematic_coordinates;
				use et_symbol_ports;
				use et_port_names;
				use et_device_name;
				
			begin
				log (text => "net " & to_string (net_name), level => log_threshold + 3);
				log_indentation_up;

				-- Loop in ports of given net and change path and y position.
				while port_cursor /= et_kicad.schematic.pac_ports_with_reference.no_element loop
					port := element (port_cursor); -- load the port as it currently is
					
					log (text => to_string (port.reference)
						& " port "
						& to_string (port.name), level => log_threshold + 4);
					log_indentation_up;

					-- show old position
					log (text => before & et_kicad_coordinates.to_string (
						position => port.coordinates, scope => et_kicad_coordinates.MODULE),
						level => log_threshold + 5);

					-- change path
					et_kicad_coordinates.set_path (port.coordinates, root);

					-- Move position from negative to positive y.
					move (port.coordinates);

					-- show new position
					log (text => now & et_kicad_coordinates.to_string (
						position => port.coordinates, scope => et_kicad_coordinates.MODULE),
						level => log_threshold + 5);

					-- replace old port by new port
					et_kicad.schematic.pac_ports_with_reference.replace_element (
						container		=> ports,
						position		=> port_cursor,
						new_item		=> port);
					
					log_indentation_down;
					next (port_cursor);
				end loop;
				
				log_indentation_down;
			end query_ports;

			
		begin -- flatten_netlist
			log (text => "netlist ...", level => log_threshold + 2);
			log_indentation_up;

			while net_cursor /= et_kicad.schematic.type_netlist.no_element loop
				
				et_kicad.schematic.type_netlist.update_element (
					container	=> module.netlist,
					position	=> net_cursor,
					process		=> query_ports'access);
				
				next (net_cursor);
			end loop;
			
			log_indentation_down;
		end flatten_netlist;

		
	begin -- transpose
		log (text => "transposing coordinates of KiCad modules ...", level => log_threshold);
		log_indentation_up;
		
		while module_cursor /= et_kicad.pcb.type_modules.no_element loop
			log (text => "module " & et_kicad_coordinates.to_string (key (module_cursor)), level => log_threshold + 1);
			log_indentation_up;

			-- log (text => "schematic ...", level => log_threshold + 1);
			-- log_indentation_up;

			-- As preparation for later moving the y positions of schematic objects,
			-- we need a list of schematic drawing frames. This list will later provide the
			-- paper sizes of individual drawing sheets of the schematic:
			schematic_frames := element (module_cursor).frames;

			-- As preparation for later moving the y positions of board objects,
			-- the layout_sheet_height must be set:
			if board_available then
				prepare_layout_y_movements;
			end if;
			
			update_element (
				container	=> et_kicad.pcb.modules,
				position	=> module_cursor,
				process		=> flatten_notes'access);

			-- CS: The design import (see function read_schematic in kicad.adb) does not read the title block and 
			-- drawing frame lines and texts. Currently only content of things like company name and comments
			-- is read.
			update_element (
				container	=> et_kicad.pcb.modules,
				position	=> module_cursor,
				process		=> flatten_frames'access);

			update_element (
				container	=> et_kicad.pcb.modules,
				position	=> module_cursor,
				process		=> flatten_components'access);

			update_element (
				container	=> et_kicad.pcb.modules,
				position	=> module_cursor,
				process		=> flatten_nets'access);

			update_element (
				container	=> et_kicad.pcb.modules,
				position	=> module_cursor,
				process		=> flatten_netlist'access);
			
			-- general non-component related board stuff (silk screen, documentation, ...):
			if board_available then
				update_element (
					container	=> et_kicad.pcb.modules,
					position	=> module_cursor,
					process		=> move_general_board_stuff'access);
			end if;
			
			next (module_cursor);
			log_indentation_down;
		end loop;
		
		log_indentation_down;
	end transpose;


	
	
	-- Converts kicad schematic coordinates to native schematic coordinates.
	function to_native_coordinates (
		point 		: in et_kicad_coordinates.type_position;
		rotation	: in et_schematic_coordinates.type_rotation_model := et_schematic_coordinates.pac_geometry_2.zero_rotation)
		return et_schematic_coordinates.type_object_position 
	is
		use et_schematic_coordinates;
		point_out : type_object_position;
	begin
		point_out := et_schematic_coordinates.to_position (
			point		=> et_kicad_coordinates.get_point (point), -- x,y
			sheet		=> et_kicad_coordinates.sheet (point), -- sheet
			rotation	=> rotation);

		return point_out;
	end;


	
	
	-- Converts shapes of symbols to native shapes:
	function convert_shapes (
		shapes			: in et_kicad_libraries.type_symbol_shapes;
		log_threshold	: in type_log_level)
		return et_symbols.type_shapes 
	is
		use et_kicad_libraries;
		
		use et_symbols;		
		native_shapes : et_symbols.type_shapes;

		procedure copy_line (cursor : in et_kicad_libraries.type_symbol_lines.cursor) is begin
			pac_symbol_lines.append (
				container	=> native_shapes.lines,
				new_item	=> et_kicad_libraries.type_symbol_lines.element (cursor));
		end;

		procedure copy_arc (cursor : in et_kicad_libraries.type_symbol_arcs.cursor) is begin
			pac_symbol_arcs.append (
				container	=> native_shapes.arcs,
				new_item	=> et_symbols.type_symbol_arc (et_kicad_libraries.type_symbol_arcs.element (cursor)));
		end;

		procedure copy_circle (cursor : in et_kicad_libraries.type_symbol_circles.cursor) is begin
			pac_symbol_circles.append (
				container	=> native_shapes.circles,
				new_item	=> (
					type_circle_base (et_kicad_libraries.type_symbol_circles.element (cursor))
					with filled => NO));
		end;						

		
		-- Converts a polyline to single lines and appends them to native_shapes.lines.
		procedure copy_polyline (cursor : in et_kicad_libraries.type_symbol_polylines.cursor) is 
			use et_kicad;
			use type_symbol_points;

			-- This is the given kicad polyline:
			polyline : type_symbol_polyline := type_symbol_polylines.element (cursor);

			-- This cursor points to a particular point of the polyline:
			point_cursor : type_symbol_points.cursor := polyline.points.first;

			-- This is the native line that will be appended to native.shapes.lines:
			line : type_symbol_line;

			-- This flag indicates whether a start or an end point of a line is expected:
			start : boolean := true; -- when start point -> true, when end point -> false
		begin
			line.width := polyline.width;
				
			-- Advance through points of polyline and assign line start and and points.
			-- Then append the line to native.shapes.lines.
			while point_cursor /= type_symbol_points.no_element loop

				case start is
					when TRUE =>
						-- The point is a start point if another point follows. Otherwise nothing happens.
						if next (point_cursor) /= type_symbol_points.no_element then
							set_A (line, element (point_cursor)); -- start point
							start := false; -- up next: end point
						end if;

					when FALSE =>
						set_B (line, element (point_cursor)); -- end point
						start := true; -- up next: start point

						-- append line to collection of native lines
						et_symbols.pac_symbol_lines.append (
							container	=> native_shapes.lines,
							new_item	=> line);

						-- Set cursor one point back so that this point serves as start point
						-- for the next segment.
						previous (point_cursor);

				end case;

				next (point_cursor); -- advance to next point
			end loop;
		end copy_polyline;

		
		-- Converts a rectangle to four lines and appends them to native_shapes.lines.
		procedure copy_rectangle (cursor : in et_kicad_libraries.type_symbol_rectangles.cursor) is
			use et_kicad;
			use type_symbol_rectangles;
			use et_schematic_coordinates;
			
			-- This is the given kicad rectangle:
			rectangle : type_symbol_rectangle := type_symbol_rectangles.element (cursor);

			-- This is the native line that will be appended to native_shapes.lines:
			line : type_symbol_line;
			
			width, height : et_schematic_coordinates.type_distance_model;

			-- These two points are required to form the final rectangle:
			corner_C, corner_D : pac_geometry_2.type_vector_model;

			
			procedure append_line is begin
				et_symbols.pac_symbol_lines.append (
					container	=> native_shapes.lines,
					new_item	=> line);
			end;

			use pac_geometry_2;

			
		begin -- copy_rectangle
			line.width := rectangle.width;
			
			log_indentation_up;
			log (text => "start " & to_string (rectangle.corner_A) 
				 & " end" & to_string (rectangle.corner_B),
				 level => log_threshold + 2);
			
			-- compute width and height of the rectangle:
			width  := get_distance (axis => AXIS_X, point_2 => rectangle.corner_B, point_1 => rectangle.corner_A);

			log (text => "width" & to_string (width), level => log_threshold + 2);
			
			if width < zero then
				rectangle.corner_A := type_vector_model (invert (rectangle.corner_A, MIRROR_ALONG_X_AXIS));
				rectangle.corner_B := type_vector_model (invert (rectangle.corner_B, MIRROR_ALONG_X_AXIS));
				width := - width;
			end if;
			
			height := get_distance (axis => AXIS_Y, point_2 => rectangle.corner_B, point_1 => rectangle.corner_A);

			log (text => "height" & to_string (height), level => log_threshold + 2);
			
			if height < zero then
				rectangle.corner_A := type_vector_model (invert (rectangle.corner_A, MIRROR_ALONG_Y_AXIS));
				rectangle.corner_B := type_vector_model (invert (rectangle.corner_B, MIRROR_ALONG_Y_AXIS));
				height := - height;
			end if;

			log (text => "new start " & to_string (rectangle.corner_A) 
				 & " new end" & to_string (rectangle.corner_B),
				 level => log_threshold + 2);
			
			-- compute corner points of the rectangle:
			-- corner_A is the lower left corner of the rectangle
			-- corner_B is the upper right corner of the rectangle

			-- corner_C is the lower right corner:
			corner_C := type_vector_model (set (
				x => get_x (rectangle.corner_A) + width,
				y => get_y (rectangle.corner_A)
				));

			-- corner_D is the upper left corner:
			corner_D := type_vector_model (set (
				x => get_x (rectangle.corner_A),
				y => get_y (rectangle.corner_A) + height
				));
			
			-- lower horizontal line
			set_A (line, rectangle.corner_A);
			set_B (line, corner_C);
			append_line;
			
			-- upper horizontal line	
			set_A (line, corner_D);
			set_B (line, rectangle.corner_B);
			append_line;

			-- left vertical line
			set_A (line, rectangle.corner_A);
			set_B (line, corner_D);
			append_line;

			-- right vertical line
			set_A (line, corner_C);
			set_B (line, rectangle.corner_B);
			append_line;

			log_indentation_down;
		end copy_rectangle;

		
	begin -- convert_shapes
		log (text => "converting shapes ...", level => log_threshold);
		log_indentation_up;

		log (text => "lines ...", level => log_threshold + 1);
		et_kicad_libraries.type_symbol_lines.iterate (shapes.lines, copy_line'access);

		log (text => "arcs ...", level => log_threshold + 1);
		et_kicad_libraries.type_symbol_arcs.iterate (shapes.arcs, copy_arc'access);

		log (text => "circles ...", level => log_threshold + 1);
		et_kicad_libraries.type_symbol_circles.iterate (shapes.circles, copy_circle'access);

		log (text => "polylines ...", level => log_threshold + 1);
		et_kicad_libraries.type_symbol_polylines.iterate (shapes.polylines, copy_polyline'access);

		log (text => "rectangles ...", level => log_threshold + 1);
		et_kicad_libraries.type_symbol_rectangles.iterate (shapes.rectangles, copy_rectangle'access);

		log_indentation_down;
		return native_shapes;
	end convert_shapes;


	
	procedure to_native (
		project_name	: in pac_project_name.bounded_string;
		log_threshold	: in type_log_level) 
	is
		use et_project;
		use et_package_names;
		
-- 		-- When the native project is created we need a project path and a project name:
-- 		project_path : et_project.type_et_project_path.bounded_string :=
-- 						et_project.type_et_project_path.to_bounded_string (
-- 							compose (et_general.work_directory, et_project.directory_import));

		prefix_devices_dir : pac_device_model_file.bounded_string := -- libraries/devices
			to_file_name (compose (
				directory_libraries, directory_libraries_devices));
	
		prefix_packages_dir : pac_package_model_file_name.bounded_string := -- libraries/packages
			to_file_name (compose (
				directory_libraries, directory_libraries_packages));

		-- Since V4 package libraries are stored in et_kicad_pcb.package_libraries
		-- the copy/convert process must be performed only once.
		-- This flag goes true once V4 package libraries have been converted.
		packages_v4_copied : boolean := false;
		
		use et_kicad.pcb.type_modules;
		module_cursor_kicad : et_kicad.pcb.type_modules.cursor := et_kicad.pcb.type_modules.first (et_kicad.pcb.modules);

		-- This is a single native target module used as scratch.
		module : type_generic_module; 

		
		-- Converts kicad texts to native texts:
		function to_texts (texts_in : et_kicad.schematic.type_texts.list) 
			return et_schematic_text.pac_texts.list 
		is
			use et_schematic_text;
			use pac_text_schematic;
			texts_out : et_schematic_text.pac_texts.list;

			procedure query_texts (cursor : in et_kicad.schematic.type_texts.cursor) is
				text_kicad : et_kicad.schematic.type_text := et_kicad.schematic.type_texts.element (cursor);
				text_native : et_schematic_text.type_text;
			begin
				-- copy the coordinates x/y, sheet and rotation from kicad text to native text
				--text_native.position := to_native_coordinates (text_kicad.position);
				--text_native.position := et_schematic_coordinates.pac_geometry_2.type_vector_model (text_kicad.position);
				text_native.position := et_kicad_coordinates.get_point (text_kicad.position);
				text_native.sheet := et_kicad_coordinates.sheet (text_kicad.position);
				text_native.rotation := snap (text_kicad.rotation);
				
				-- copy the content
				text_native.content := text_kicad.content;

				-- append native text to list of native texts
				et_schematic_text.pac_texts.append (
					container	=> texts_out,
					new_item	=> text_native);

			end query_texts;
			
		begin -- to_texts
			et_kicad.schematic.type_texts.iterate (texts_in, query_texts'access);
			return texts_out;
		end;


		
		procedure copy_general_stuff is begin
			module.board_available	:= element (module_cursor_kicad).board_available;
			module.texts			:= to_texts (element (module_cursor_kicad).notes); 
			module.board			:= et_pcb.type_board (element (module_cursor_kicad).board);
			module.net_classes		:= element (module_cursor_kicad).net_classes;
		end copy_general_stuff;


		
		-- The return is a composition of prefix_devices_dir, library containing directory,
		-- generic component name and device model extension 
		-- like: libraries/devices/__-__-lbr-bel_logic_7400.dev
		function concatenate_lib_name_and_generic_name (
			library	: in pac_device_model_file.bounded_string; -- ../../lbr/bel_logic.lib
			device	: in et_kicad_libraries.type_component_generic_name.bounded_string) -- 7400
			return pac_device_model_file.bounded_string
		is

			use pac_device_model_file;
			dir : pac_device_model_file.bounded_string; -- ../../lbr
			name : pac_device_model_file.bounded_string; -- to be returned -- libraries/devices/__-__-lbr-bel_logic_7400.dev

			-- In the containing directory . and / must be replaced by _ and -:
			characters : character_mapping := to_mapping ("./","_-");
			
		begin -- concatenate_lib_name_and_generic_name
			dir := to_file_name (containing_directory (to_string (name => library)) & '-'); -- "..-..-lbr"
								 
			pac_device_model_file.translate (dir, characters); -- __-__-lbr
			--log (text => "dir " & et_libraries.to_string (dir));
			
			name := to_file_name (base_name (to_string (name => library))); -- bel_logic
			name := dir & name;
			--log (text => "name " & et_libraries.to_string (name));

			name := name & '_' & to_file_name (et_kicad_libraries.to_string (device));
			--log (text => "name " & et_libraries.to_string (name));

			name := to_file_name (compose (
					containing_directory	=> to_string (name => prefix_devices_dir),
					name					=> to_string (name => name),
					extension				=> device_model_file_extension));

			--log (text => "name " & et_libraries.to_string (name));
			
			return name;
		end concatenate_lib_name_and_generic_name;

		

		function rename_package_model (
			model_in : in pac_package_model_file_name.bounded_string) -- ../../lbr/transistors.pretty/S_0805
			return pac_package_model_file_name.bounded_string 
		is
			-- The return is something like: libraries/packages/__-__-lbr-transistors.pretty_S_0805.pac .

			use et_package_names;
			use pac_package_model_file_name;

			-- In the containing directory . and / must be replaced by _ and -:
			characters : character_mapping := to_mapping ("./","_-");

			model_copy : pac_package_model_file_name.bounded_string := model_in; -- ../../lbr/transistors.pretty/S_0805
			model_return : pac_package_model_file_name.bounded_string;
		begin
			translate (model_copy, characters);

			model_return := to_file_name (compose (
					containing_directory	=> to_string (name => prefix_packages_dir),
					name					=> to_string (name => model_copy),
					extension				=> package_model_file_extension));

			return model_return;
		end rename_package_model;


		
		-- Transfer components from kicad module to native module.
		-- Changes the links to device models so that they point to the libraries
		-- in project/libraries/devices/...
		procedure copy_components is
			use et_devices_electrical;
			use et_device_appearance;
			use et_device_purpose;
			use et_device_name;			
			use et_kicad.schematic.type_components_schematic;
			
			components_kicad		: et_kicad.schematic.type_components_schematic.map;
			component_cursor_kicad	: et_kicad.schematic.type_components_schematic.cursor;

			use pac_devices_sch;
			component_cursor_native	: pac_devices_sch.cursor;
			component_inserted		: boolean;

			
			-- Copies the kicad units to the native component.
			procedure copy_units (
				reference	: in type_device_name;
				component	: in out type_device_sch) 
			is
				use et_kicad.schematic.type_units_schematic;
				units_kicad			: et_kicad.schematic.type_units_schematic.map := element (component_cursor_kicad).units;
				unit_cursor_kicad	: et_kicad.schematic.type_units_schematic.cursor := units_kicad.first; -- point to first unit

				use et_units.pac_units;
				unit_cursor_native	: et_units.pac_units.cursor;
				unit_inserted		: boolean;

				unit_native_virtual	: et_units.type_unit (APPEARANCE_VIRTUAL);
				unit_native_real	: et_units.type_unit (APPEARANCE_PCB);

				use pac_unit_name;
				use et_object_status;
			begin
				log_indentation_up;
				
				while unit_cursor_kicad /= et_kicad.schematic.type_units_schematic.no_element loop
					log (text => "unit " & to_string (key (unit_cursor_kicad)), level => log_threshold + 3);

					-- depending on the appearance of the kicad component, we create a virtual or real 
					-- unit in the native schematic module:

					-- The units can be obtained by converting the kicad unit to the base unit
					-- and adding stuff of real components (if real device).
					-- Kicad stuff like "alternative representation", package name, datasheet is discarded.
					case element (component_cursor_kicad).appearance is
						when APPEARANCE_VIRTUAL =>

							unit_native_virtual := (
								mirror		=> element (unit_cursor_kicad).mirror,
								status		=> get_default_status,					   
								position	=> to_native_coordinates (
												point		=> element (unit_cursor_kicad).position,
												rotation 	=> element (unit_cursor_kicad).rotation),
								appearance	=> APPEARANCE_VIRTUAL);
							
							et_units.pac_units.insert (
								container	=> component.units,
								key			=> key (unit_cursor_kicad),
								position	=> unit_cursor_native,
								inserted	=> unit_inserted,
								new_item	=> unit_native_virtual);

						when APPEARANCE_PCB => -- real device

							unit_native_real := (
								mirror		=> element (unit_cursor_kicad).mirror,
								status		=> get_default_status,
								position	=> to_native_coordinates (
												point		=> element (unit_cursor_kicad).position,
												rotation 	=> element (unit_cursor_kicad).rotation),
								appearance	=> APPEARANCE_PCB,
							
								-- and stuff that comes with a real device:
									
								name		=> element (unit_cursor_kicad).reference,
								value		=> element (unit_cursor_kicad).value,
												
								-- create a placeholder for purpose because kicad does not know such a thing
								purpose		=> (meaning => PURPOSE, others => <>)
								);
							
							et_units.pac_units.insert (
								container	=> component.units,
								key			=> key (unit_cursor_kicad),
								position	=> unit_cursor_native,
								inserted	=> unit_inserted,
								new_item	=> unit_native_real);
								
					end case;
					
					next (unit_cursor_kicad);
				end loop;
				
				log_indentation_down;
			end copy_units;

			
		begin -- copy_components
			log (text => "copying schematic devices ...", level => log_threshold + 1);
			log_indentation_up;
			
			-- load a copy of kicad schematic components
			components_kicad := element (module_cursor_kicad).components;
			
			-- loop in the component list of the kicad schematic module
			component_cursor_kicad := components_kicad.first;
			while component_cursor_kicad /= et_kicad.schematic.type_components_schematic.no_element loop

				log (text => to_string (key (component_cursor_kicad)), level => log_threshold + 2);
				
				-- depending on the appearance of the kicad component, we create a virtual or real 
				-- component in the native schematic module.
				-- Kicad stuff like the boolean power_flag is ignored.
				case element (component_cursor_kicad).appearance is
					when APPEARANCE_VIRTUAL =>
						
						pac_devices_sch.insert (
							container	=> module.devices,
							key			=> key (component_cursor_kicad), -- PWR04, FLG01
							position	=> component_cursor_native,
							new_item	=> (
								appearance	=> APPEARANCE_VIRTUAL,

								-- The link to the device model is a composition of path,file and generic name:
								model		=> concatenate_lib_name_and_generic_name (
												library	=> element (component_cursor_kicad).library_name,
												device	=> element (component_cursor_kicad).generic_name),

								-- NOTE: The value of virtual components (like power symbols) is discarded here.
								
								others 		=> <>), -- unit list is empty at this time

							inserted	=> component_inserted); -- should always be true

						
					when APPEARANCE_PCB =>
-- 						log (text => "placeholders silk top" & count_type'image (et_packages.pac_text_placeholders.length (
-- 							element (component_cursor_kicad).text_placeholders.silkscreen.top)));
-- 
-- 						log (text => "placeholders silk bottom" & count_type'image (et_packages.pac_text_placeholders.length (
-- 							element (component_cursor_kicad).text_placeholders.silkscreen.bottom)));
-- 
-- 						log (text => "placeholders assy top" & count_type'image (et_packages.pac_text_placeholders.length (
-- 							element (component_cursor_kicad).text_placeholders.assy_doc.top)));
-- 
-- 						log (text => "placeholders assy bottom" & count_type'image (et_packages.pac_text_placeholders.length (
-- 							element (component_cursor_kicad).text_placeholders.assy_doc.bottom)));
						
						pac_devices_sch.insert (
							container	=> module.devices,
							key			=> key (component_cursor_kicad), -- IC308, R12
							position	=> component_cursor_native,
							new_item	=> (
								appearance	=> APPEARANCE_PCB,

								-- The link to the device model is a composition of path,file and generic name:
								model		=> concatenate_lib_name_and_generic_name (
												library	=> element (component_cursor_kicad).library_name,
												device	=> element (component_cursor_kicad).generic_name),

								value		=> element (component_cursor_kicad).value,
								partcode	=> to_partcode (partcode_default), -- not provided by kicad
								purpose		=> to_purpose (purpose_default), -- not provided by kicad
								variant		=> element (component_cursor_kicad).variant,

								position			=> element (component_cursor_kicad).position,
								text_placeholders	=> element (component_cursor_kicad).text_placeholders,
								others 				=> <>), -- unit list is empty at this time

							inserted	=> component_inserted); -- should always be true
				end case;

				-- copy the units from the kicad component to the native device
				pac_devices_sch.update_element (
					container	=> module.devices,
					position	=> component_cursor_native,
					process		=> copy_units'access);

				next (component_cursor_kicad);

			end loop;

			log_indentation_down;
		end copy_components;


		
		
		procedure copy_nets is
			
			use et_kicad.schematic.type_nets;
			use et_kicad.schematic.type_strands;
			kicad_nets			: et_kicad.schematic.type_nets.map := element (module_cursor_kicad).nets;
			kicad_net_cursor	: et_kicad.schematic.type_nets.cursor := kicad_nets.first;

			use et_nets.pac_nets;
			net_cursor_native	: et_nets.pac_nets.cursor;
			net_inserted		: boolean;

			
			procedure insert_strands (
			-- copies the kicad strands to native strands of a net.
			-- Strand names (from kicad) are discarded. ET does not provide a name for a strand.
			-- As a strand is part of a net, there is no need for individual strand names.
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_nets.type_net) 
			is
				use et_kicad.schematic.type_strands;
				kicad_strands : et_kicad.schematic.type_strands.list := element (kicad_net_cursor).strands;
				kicad_strand_cursor : et_kicad.schematic.type_strands.cursor := kicad_strands.first;
								
				use et_kicad.schematic.type_net_segments;
				kicad_segments : et_kicad.schematic.type_net_segments.list;
				kicad_segment_cursor : et_kicad.schematic.type_net_segments.cursor;

				use et_nets.pac_strands;
				strands_native : et_nets.pac_strands.list;
				strand_native : et_nets.type_strand;

				use et_net_segment;
				use et_net_segment.pac_net_segments;
				net_segments_native : et_net_segment.pac_net_segments.list;
				net_segment_native : et_net_segment.type_net_segment;

				use et_net_labels.pac_net_labels;
				use et_net_ports.pac_device_ports;

				
				-- Copies from the given kicad net segment all simple and tag labels and returns
				-- them in a single list.
				-- CS: Labels placed at 180 or 270 degree are rotated to 0 or 90 degree. This might
				-- cause the labels to shift to the right or up.
				function tag_and_simple_labels (segment : in et_kicad.schematic.type_net_segment) 
					return et_net_labels.pac_net_labels.list 
				is
					use et_schematic_text;
					use pac_text_schematic;
					
					labels : et_net_labels.pac_net_labels.list; -- to be returned

					use et_kicad.schematic.type_simple_labels;
					simple_label_cursor : et_kicad.schematic.type_simple_labels.cursor := segment.label_list_simple.first;

					use et_schematic_coordinates;
					use et_schematic_coordinates.pac_geometry_2;

					-- Simple labels require to be shifted slightly both to the right and up.
					-- This prevents them to sit directly on the net segment:
					-- CS: The offset should depend on the direction of the segment.
					-- Currently this simple minded approach shifts to the right and up
					-- no matter whether the segment runs horizontally or vertically.
					offset : constant type_vector_model := to_vector_model (x => 1.0, y => 1.0);

					-- the new label position after applying the offset:
					simple_label_position : type_vector_model;
					
					use et_kicad.schematic.type_tag_labels;
					tag_label_cursor : et_kicad.schematic.type_tag_labels.cursor := segment.label_list_tag.first;

					
					-- Kicad label can be rotated by 180 or -90 degree. This function translates 
					-- to native label rotation:
					function to_rotation (rk : in type_rotation_relative) 
						return et_text.type_rotation_documentation 
					is 
						use et_text;
					begin
						if rk = 180.0 then
							return HORIZONTAL;
						
						elsif rk = zero_rotation then
							return HORIZONTAL;

						elsif rk = -90.0 then
							return VERTICAL;

						elsif rk = 90.0 then
							return VERTICAL;

						else
							-- CS: all other angles are converted to zero degree.
							warning_rotation_outside_range;
							return HORIZONTAL;
						end if;
					end to_rotation;
					
					
				begin -- tag_and_simple_labels
					log_indentation_up;

					
					-- simple labels
					while simple_label_cursor /= et_kicad.schematic.type_simple_labels.no_element loop
						
						log (text => "simple label" & et_kicad.schematic.to_string (
								label => et_kicad.schematic.type_net_label (element (simple_label_cursor))),
							level => log_threshold + 5);

						-- move label by offset
						simple_label_position := element (simple_label_cursor).coordinates;
						move_by (simple_label_position, offset);

						et_net_labels.pac_net_labels.append (
							container	=> labels,
							new_item	=> (
								position	=> simple_label_position,
								rotation	=> to_rotation (rk => element (simple_label_cursor).rotation),
								size		=> element (simple_label_cursor).size,
								width		=> element (simple_label_cursor).width,
								others		=> <>));
						
						next (simple_label_cursor);
					end loop;

					
					-- tag labels
					while tag_label_cursor /= et_kicad.schematic.type_tag_labels.no_element loop

						log (text => "tag label" & et_kicad.schematic.to_string (
							label => et_kicad.schematic.type_net_label (element (tag_label_cursor))),
							 level => log_threshold + 5);

						-- CS: Here we convert a tag label to a native simple label
						-- as a makeshift. The tag label should be attached to the
						-- corresponding net segment instead:
						et_net_labels.pac_net_labels.append (
							container	=> labels,
							new_item	=> (
								position	=> element (tag_label_cursor).coordinates,
								-- rotation	=> element (tag_label_cursor).rotation,
								size		=> element (tag_label_cursor).size,
								width		=> element (tag_label_cursor).width,
								-- direction	=> element (tag_label_cursor).direction,
								others		=> <>));
						
						next (tag_label_cursor);
					end loop;

					log_indentation_down;
					return labels;
				end tag_and_simple_labels;


				
				function read_net_junctions (segment : in et_kicad.schematic.type_net_segment)
				-- Iterates junctions of segment. Tests if they sit on start or end point
				-- and sets the corresponding flag in the native junctions of the native segment.
				-- Issues warning on misplaced junction. The misplaced junction is discarded.
				-- By this mechanism excessive junctions (sitting on top of each other) 
				-- are reduced to a single junction (without warning).
				-- CS: NOTE: Misleading warnings may be issued here due to improper junction processing
				-- in procedure et_kicad.schematic.process_junctions.
					return et_net_junction.type_junctions 
				is
					junctions : et_net_junction.type_junctions; -- to be returned

					use et_schematic_coordinates.pac_geometry_2;
					use et_kicad_coordinates;
					use et_kicad.schematic.type_junctions;
					junction_cursor : et_kicad.schematic.type_junctions.cursor := segment.junctions.first;
					
				begin
					log_indentation_up;
					
					while junction_cursor /= et_kicad.schematic.type_junctions.no_element loop

						log (text => "junction" & to_string (
							get_point (element (junction_cursor).coordinates)),
							level => log_threshold + 5);

						-- Test if junction sits at start point of segment:
						if element (junction_cursor).coordinates = segment.coordinates_start then
							junctions.A := true;

						-- Test if junction sits at end point of segment:
						elsif element (junction_cursor).coordinates = segment.coordinates_end then
							junctions.B := true;

						-- If junction misplaced, issue warning:
						else
							log (WARNING, 
								"misplaced junction between start and end point of segment -> ignored !");
						end if;
						
						next (junction_cursor);
					end loop;
					
					log_indentation_down;
					return junctions;
				end read_net_junctions;


				
				-- Returns the component ports connected with the given net segment
				-- at the given end (A or B):
				function read_ports (
					segment : in et_kicad.schematic.type_net_segment;
					AB_end	: in et_schematic_coordinates.pac_geometry_2.type_start_end_point)
					return et_net_ports.pac_device_ports.set 
				is
					use et_kicad.schematic;
					use et_kicad.schematic.pac_ports_with_reference;
					port_cursor_kicad	: pac_ports_with_reference.cursor;
					all_ports_of_net	: pac_ports_with_reference.set;
					
					ports_of_segment : et_net_ports.pac_device_ports.set; -- to be returned

					use et_sheets;
					use et_schematic_coordinates;
					use pac_geometry_sch;
					use pac_geometry_2;
					use et_symbol_ports;
					use et_port_names;
					use et_device_name;
					use pac_unit_name;
					use et_package_variant;
					
					dist : pac_geometry_sch.type_distance_point_line;

					terminal : type_terminal;
				begin
					log_indentation_up;
					
					-- Get all ports connected with the current net (in the kicad module):
					-- CS: No need to do that on every net segment anew. Move up one level.
					all_ports_of_net := components_in_net (
						module			=> key (module_cursor_kicad), -- the name of the kicad module
						net				=> net_name, -- the net in question 
						log_threshold	=> log_threshold + 6);

					-- Loop in all ports of the net. 
					-- Select the ports which are on the same sheet as the current strand.
					-- Select the ports which are connected with the given net segment.
					port_cursor_kicad := all_ports_of_net.first;
					while port_cursor_kicad /= pac_ports_with_reference.no_element loop

						-- compare sheet numbers
						if 	et_kicad_coordinates.sheet (element (port_cursor_kicad).coordinates) = 
							et_kicad_coordinates.sheet (element (kicad_strand_cursor).position) then

							declare
								line : pac_geometry_2.type_line := type_line (to_line (
									A	=> et_kicad_coordinates.get_point (segment.coordinates_start), 
									B	=> et_kicad_coordinates.get_point (segment.coordinates_end)));

								-- Get the position of the port:
								position : type_vector_model := 
									et_kicad_coordinates.get_point (element (port_cursor_kicad).coordinates);
							begin
								-- If port sits on segment, append it to ports_of_segment.
								if on_line (line, position) then				

									-- Get the name of the unit:
									terminal := to_terminal (
											port			=> element (port_cursor_kicad),
											module			=> key (module_cursor_kicad),  -- the name of the kicad module
											log_threshold	=> log_threshold + 6);

									log (text => to_string (element (port_cursor_kicad).reference) 
										& " unit " & to_string (terminal.unit)
										& " port "
										& to_string (element (port_cursor_kicad).name)
										& et_kicad_coordinates.to_string (
												position	=> element (port_cursor_kicad).coordinates,
												scope		=> et_kicad_coordinates.XY),
										level => log_threshold + 5);


									case AB_end is
										when A =>
											if get_A (line) = position then
												
												et_net_ports.pac_device_ports.insert (
													container	=> ports_of_segment,
													new_item	=> (
														device_name	=> element (port_cursor_kicad).reference,
														unit_name	=> terminal.unit, -- IO-BANK1, C, A, ...
														port_name	=> element (port_cursor_kicad).name));

											end if;

										when B =>
											if get_B (line) = position then

												et_net_ports.pac_device_ports.insert (
													container	=> ports_of_segment,
													new_item	=> (
														device_name	=> element (port_cursor_kicad).reference,
														unit_name	=> terminal.unit, -- IO-BANK1, C, A, ...
														port_name	=> element (port_cursor_kicad).name));

											end if;
									end case;
								end if;
							end;

						end if;
						next (port_cursor_kicad);
					end loop;
					
					log_indentation_down;
					return ports_of_segment;
				end read_ports;

				
			begin -- insert_strands
				log_indentation_up;
				
				-- loop in strands of current kicad net
				while kicad_strand_cursor /= et_kicad.schematic.type_strands.no_element loop
					log (text => "strand" & et_kicad_coordinates.to_string (
						 position	=> element (kicad_strand_cursor).position,
						 scope		=> et_kicad_coordinates.SHEET),
						 level => log_threshold + 3);
					
					-- load segments of current strand
					kicad_segments := element (kicad_strand_cursor).segments;
					kicad_segment_cursor := kicad_segments.first;

					-- loop in segments of current strand
					-- A kicad net segment has labels and junctions.
					log_indentation_up;
					
					while kicad_segment_cursor /= et_kicad.schematic.type_net_segments.no_element loop

						log (text => "segment" & et_kicad.schematic.to_string (
							segment		=> element (kicad_segment_cursor),
							scope		=> et_kicad_coordinates.XY),
							level => log_threshold + 4);
						
						-- get coordinates from current kicad net segment:
						set_A (net_segment_native, et_kicad_coordinates.get_point (element (kicad_segment_cursor).coordinates_start));
						set_B (net_segment_native, et_kicad_coordinates.get_point (element (kicad_segment_cursor).coordinates_end));

						-- get labels from current kicad net segment
						net_segment_native.labels := tag_and_simple_labels (element (kicad_segment_cursor));

						-- read net junctions of the current segment
						net_segment_native.junctions := read_net_junctions (element (kicad_segment_cursor));

						
						-- Read ports connected with the A and B end of the candiate segment:
						net_segment_native.ports.A.devices := 
							read_ports (element (kicad_segment_cursor), et_schematic_coordinates.pac_geometry_2.A);

						net_segment_native.ports.B.devices := 
							read_ports (element (kicad_segment_cursor), et_schematic_coordinates.pac_geometry_2.B);

						
						-- there are no ports of submodules
						-- CS net_segment_native.ports.submodules := et_net_segment.pac_submodule_ports.empty_set;
						
						-- Collect native net segment in list net_segments_native.
						et_net_segment.pac_net_segments.append (
							container	=> net_segments_native,
							new_item	=> net_segment_native);

						next (kicad_segment_cursor);
					end loop;
					
					log_indentation_down;
					

					-- copy net segments to native strand
					strand_native.segments := net_segments_native;

					
					-- copy sheet number from kicad strand to native strand:
					et_schematic_coordinates.set_sheet (strand_native.position, et_kicad_coordinates.sheet (element (kicad_strand_cursor).position));

					-- calculate lowest x/y of native strand
					et_nets.set_strand_position (strand_native);


					
					-- clear collection of net segments (for the next strand)
					clear (net_segments_native);
					
					-- collect native strand (incl. segments) in list strands_native
					et_nets.pac_strands.append (
						container	=> strands_native,
						new_item	=> strand_native);
					
					next (kicad_strand_cursor);
				end loop;

				net.strands := strands_native;

				log_indentation_down;
			end insert_strands;


			
			
			procedure copy_layout_stuff (
				net_name	: in pac_net_name.bounded_string;
				net			: in out et_nets.type_net) 
			is 
				use et_net_class;
			begin
				log_indentation_up;

				log (text => "class" & to_string (element (kicad_net_cursor).class), level => log_threshold + 3);
				net.class := element (kicad_net_cursor).class;	
				
				log (text => "tracks, vias, polygons ...", level => log_threshold + 3);
				net.route := element (kicad_net_cursor).route;
				-- CS log details on tracks, vias, ...
				
				log_indentation_down;
			end copy_layout_stuff;

			
		begin -- copy_nets
			-- loop in kicad nets
			while kicad_net_cursor /= et_kicad.schematic.type_nets.no_element loop
				log (text => "net " & to_string (key (kicad_net_cursor)), level => log_threshold + 2);

				et_nets.pac_nets.insert (
					container	=> module.nets,
					position	=> net_cursor_native,
					inserted	=> net_inserted,
					key			=> key (kicad_net_cursor), -- net name
					new_item	=> (
							-- convert the kicad net scope to native net scope
							scope	=> et_netlists.to_net_scope (et_kicad.schematic.to_string (
										element (kicad_net_cursor).scope)),
							others 	=> <>)
					);

				-- insert strands (schematic related)
				et_nets.pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor_native,
					process		=> insert_strands'access);

				-- copy layout related stuff (copper segments, vias, ...)
				et_nets.pac_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor_native,
					process		=> copy_layout_stuff'access);
				
				next (kicad_net_cursor);
			end loop;
		end copy_nets;
		

		
		-- Converts the kicad drawing frame templates (schematic and layout) to native templates.
		-- CS: not completed yet.
		-- For the time being the native module gets dummy templates assigned.
		procedure copy_frames is
			use et_drawing_frame.schematic;
			use et_drawing_frame.board;
		begin
			-- schematic frames:
			module.frames.template := template_schematic_default;

			-- board frame:
			module.board.frame.template := template_pcb_default;
		end copy_frames;


		
		procedure copy_libraries (
			module_name : in et_kicad_coordinates.type_submodule_name.bounded_string;
			module		: in et_kicad.pcb.type_module) 
		is			
			-- This cursor points to the kicad component library being converted:
			use et_kicad_libraries.type_device_libraries;
			component_library_cursor : et_kicad_libraries.type_device_libraries.cursor := module.component_libraries.first;

			use pac_device_model_file;
			component_library_name : pac_device_model_file.bounded_string; -- lbr/logic.lib

			-- This cursor points to the kicad footprint library being converted:			
			use et_kicad_packages.type_libraries;
			package_library_cursor : et_kicad_packages.type_libraries.cursor := module.footprints.first;

			-- use pac_package_model_file_name;

			
			procedure query_components (
				library_name	: in pac_device_model_file.bounded_string; -- lbr/logic.lib
				library			: in et_kicad_libraries.type_components_library.map) 
			is				
				use et_symbols;
				use et_device_appearance;
				use et_kicad_libraries.type_components_library;
				component_cursor : et_kicad_libraries.type_components_library.cursor := library.first;

				use et_kicad_libraries.type_component_generic_name;
				generic_name : et_kicad_libraries.type_component_generic_name.bounded_string; -- 7400
				device_model : pac_device_model_file.bounded_string; -- ../lbr/logic_ttl/7400.dev

				device_cursor : pac_devices_lib.cursor;
				inserted : boolean;


				
				-- Transfers the kicad units to native units in the current native ET device.
				procedure copy_units (
					device_name	: in pac_device_model_file.bounded_string; -- libraries/devices/transistors/pnp.dev
					device		: in out type_device_model) 
				is
					use et_kicad_libraries;
					
					-- Make a copy of the kicad units of the current kicad component:
					units_kicad : et_kicad_libraries.type_units_library.map := element (component_cursor).units;

					-- This cursor points to a kicad unit:
					use et_kicad_libraries.type_units_library;
					unit_cursor_kicad : et_kicad_libraries.type_units_library.cursor := units_kicad.first;

					-- Here we store temporarily the ports of a kicad unit:
					ports_kicad : et_kicad_libraries.type_ports_library.list;
					
					-- This cursor points to a native ET unit.
					unit_cursor : pac_units_internal.cursor;
					inserted	: boolean;

					
					procedure copy_ports (
						unit_name	: in pac_unit_name.bounded_string;
						unit		: in out type_unit_internal) 
					is
						use et_symbol_ports;
						use et_port_sensitivity;
						use et_logic;
						use et_power_sources;
						

						-- Maps from kicad port style to native port characteristic.
						function to_level (style : in type_port_style) 
							return type_sensitivity_level 
						is 
							use et_kicad;
						begin
							case style is
								when INVERTED | INVISIBLE_INVERTED | INVISIBLE_INPUT_LOW |
									INPUT_LOW | OUTPUT_LOW | INVISIBLE_OUTPUT_LOW => return LOW;
								
								when others => return HIGH;
							end case;
						end to_level;

						
						-- Maps from kicad port style to native port characteristic.
						function to_edge (style : in type_port_style) 
							return type_sensitivity_edge 
						is 
							use et_kicad;
						begin
							case style is
								when CLOCK | INVISIBLE_CLOCK | RISING_EDGE_CLK |
									INVISIBLE_RISING_EDGE_CLK => return RISING;
								
								when INVERTED_CLOCK | INVISIBLE_INVERTED_CLOCK | INVISIBLE_CLOCK_LOW |
									FALLING_EDGE_CLK | INVISIBLE_FALLING_EDGE_CLK => return FALLING;
								
								when others => return NONE;
							end case;
						end to_edge;

						
						-- Maps from kicad port style to native port characteristic.
						function to_inverted (style : in type_port_style) 
							return type_output_inverted 
						is 
							use et_kicad;
						begin
							case style is
								when INVERTED | OUTPUT_LOW | INVISIBLE_INVERTED | 
									INVISIBLE_OUTPUT_LOW => return YES;
								
								when others => return NO;
							end case;
						end to_inverted;

						
						-- This cursor points to a port of a kicad unit. We initialize it so that
						-- it points to the first port of the current unit.
						use type_ports_library;
						port_cursor_kicad : type_ports_library.cursor := ports_kicad.first;

						port_inserted : boolean;
						port_cursor : pac_ports.cursor;


						use et_port_direction;
						use et_port_strength;
						
					begin -- copy_ports
						-- Loop in kicad ports and append them to the current native unit portlist.
						-- Depending on the direction and style, the kicad port gets translated to a native port.
						-- If the kicad component comes with multiple ports of the same name, only the first
						-- port is copied. A symbol is an abstraction of a function block. There is no need for 
						-- multiple ports having the same name.
						while port_cursor_kicad /= et_kicad_libraries.type_ports_library.no_element loop

							case element (port_cursor_kicad).direction is
								when PASSIVE | UNKNOWN =>
									pac_ports.insert (
										container	=> unit.symbol.ports,
										key			=> element (port_cursor_kicad).name,
										position	=> port_cursor,
										inserted	=> port_inserted,
										new_item	=> (type_port_base (element (port_cursor_kicad)) with
														direction => PASSIVE));

								when INPUT =>
									case element (port_cursor_kicad).style is
										when NON_LOGIC | INVISIBLE_NON_LOGIC =>
											pac_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (type_port_base (element (port_cursor_kicad)) with
																direction => INPUT_ANALOG));

										when others => -- all other styles indicate a digital input
											pac_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,																			   
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (type_port_base (element (port_cursor_kicad)) with
													direction			=> INPUT_DIGITAL,
													sensitivity_edge	=> to_edge (element (port_cursor_kicad).style),
													sensitivity_level	=> to_level (element (port_cursor_kicad).style)));
									end case;

									
								when OUTPUT =>
									case element (port_cursor_kicad).style is
										when NON_LOGIC | INVISIBLE_NON_LOGIC =>
											pac_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_ANALOG,
													output_analog_tristate	=> NO,
													output_analog_weakness	=> NONE));

										when others => -- all other styles indicate a digital output
											pac_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_DIGITAL,
													output_digital_inverted => to_inverted (element (port_cursor_kicad).style),
													output_digital_tristate => NO,											
													output_digital_weakness => NONE));
									end case;
											
								when TRISTATE =>
									case element (port_cursor_kicad).style is
										when NON_LOGIC | INVISIBLE_NON_LOGIC =>
											pac_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_ANALOG,
													output_analog_tristate	=> YES,
													output_analog_weakness	=> NONE));
											
										when others => -- all other styles indicate a digital output
											pac_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_DIGITAL,
													output_digital_inverted => to_inverted (element (port_cursor_kicad).style),
													output_digital_tristate => YES,
													output_digital_weakness => NONE));
									end case;
									
								when WEAK0 =>
									case element (port_cursor_kicad).style is
										when NON_LOGIC | INVISIBLE_NON_LOGIC =>
											pac_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_ANALOG,
													output_analog_tristate	=> NO,
													output_analog_weakness	=> WEAK0));
											
										when others => -- all other styles indicate a digital output
											pac_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_DIGITAL,
													output_digital_inverted	=> to_inverted (element (port_cursor_kicad).style),
													output_digital_tristate	=> NO,
													output_digital_weakness	=> WEAK0));
									end case;
									
								when WEAK1 =>
									case element (port_cursor_kicad).style is
										when NON_LOGIC | INVISIBLE_NON_LOGIC =>
											pac_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_ANALOG,
													output_analog_tristate	=> NO,
													output_analog_weakness	=> WEAK1));
											
										when others => -- all other styles indicate a digital output
											pac_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_DIGITAL,
													output_digital_inverted	=> to_inverted (element (port_cursor_kicad).style),
													output_digital_tristate	=> NO,
													output_digital_weakness	=> WEAK1));
									end case;
									
								when BIDIR =>
									pac_ports.insert (
										container	=> unit.symbol.ports,
										key			=> element (port_cursor_kicad).name,
										position	=> port_cursor,
										inserted	=> port_inserted,
										new_item	=> (type_port_base (element (port_cursor_kicad)) with
											direction			=> BIDIR_DIGITAL,
											output_inverted		=> to_inverted (element (port_cursor_kicad).style),
											output_tristate		=> NO,
											output_weakness		=> NONE,
											input_sensitivity_edge	=> to_edge (element (port_cursor_kicad).style),
											input_sensitivity_level	=> to_level (element (port_cursor_kicad).style)));

								when POWER_OUT =>
									pac_ports.insert (
										container	=> unit.symbol.ports,
										key			=> element (port_cursor_kicad).name,
										position	=> port_cursor,
										inserted	=> port_inserted,
										new_item	=> (type_port_base (element (port_cursor_kicad)) with
											direction			=> POWER_OUT,
											level				=> LEVEL_ZERO)); 
											-- CS: The level could be reasoned from the port name such as +12V or -5V.

								when POWER_IN =>
									pac_ports.insert (
										container	=> unit.symbol.ports,
										key			=> element (port_cursor_kicad).name,
										position	=> port_cursor,
										inserted	=> port_inserted,
										new_item	=> (type_port_base (element (port_cursor_kicad)) with
											direction			=> POWER_IN,
											level				=> LEVEL_ZERO)); 
											-- CS: The level could be reasoned from the port name such as +12V or -5V.

								when NOT_CONNECTED =>
									pac_ports.insert (
										container	=> unit.symbol.ports,
										key			=> element (port_cursor_kicad).name,
										position	=> port_cursor,
										inserted	=> port_inserted,
										new_item	=> (type_port_base (element (port_cursor_kicad)) with
											direction			=> NOT_CONNECTED));

									-- NOTE: The kicad port_name_offset is discarded here.

							end case;
							
							next (port_cursor_kicad);
						end loop;
					end copy_ports;


					use pac_unit_name;

					
				begin -- copy_units
					while unit_cursor_kicad /= et_kicad_libraries.type_units_library.no_element loop
						
						log (text => "unit " & enclose_in_quotes (to_string (key (unit_cursor_kicad))),
							 level => log_threshold + 4);
						log_indentation_up;
						
						-- Copy the portlist of the current unit. It is required when ports are inserted in the native unit.
						ports_kicad := element (unit_cursor_kicad).symbol.ports;

						-- create internal units
						case element (unit_cursor_kicad).appearance is
							when APPEARANCE_PCB => -- real
						
								pac_units_internal.insert (
									container	=> device.units_internal,
									key			=> key (unit_cursor_kicad), -- the name of the unit
									position	=> unit_cursor, -- set unit_cursor for later updating the current unit
									inserted	=> inserted,
									new_item	=> (
										appearance	=> APPEARANCE_PCB, -- real !
										position	=> element (unit_cursor_kicad).coordinates,
										swap_level	=> <>,
										add_level	=> <>, -- CS depends on the "global" flag. When true add_level should be "request"

										-- If the unit is real, then the symbol is real too:
										symbol		=> (type_symbol_base (element (unit_cursor_kicad).symbol)
											with 
												shapes		=> convert_shapes (element (unit_cursor_kicad).symbol.shapes, log_threshold + 5),
												appearance	=> APPEARANCE_PCB,
												ports		=> et_symbol_ports.pac_ports.empty_map, -- ports will come later
												name		=> element (unit_cursor_kicad).symbol.name, 	-- placeholder
												value		=> element (unit_cursor_kicad).symbol.value,	-- placeholder
												purpose		=> ( -- we must invent a placeholder for purpose since kicad does not know such a thing
																meaning	=> PURPOSE,
																others 	=> <>),
												-- NOTE: Other placeholders (fields in kicad) discarded here.
												
												others		=> <>)
										));

							when APPEARANCE_VIRTUAL => -- virtual unit
								
								pac_units_internal.insert (
									container	=> device.units_internal,
									key			=> key (unit_cursor_kicad), -- the name of the unit
									position	=> unit_cursor, -- set unit_cursor for later updating the current unit
									inserted	=> inserted,
									new_item	=> (
										appearance	=> APPEARANCE_VIRTUAL, -- !!
										position	=> element (unit_cursor_kicad).coordinates,
										swap_level	=> <>,
										add_level	=> <>, -- CS depends on the "global" flag. When true add_level should be "request"

										-- If the unit is virtual, then the symbol is virtual too:
										symbol		=> (type_symbol_base (element (unit_cursor_kicad).symbol)
											with 
												shapes		=> convert_shapes (element (unit_cursor_kicad).symbol.shapes, log_threshold + 5),
												appearance	=> APPEARANCE_VIRTUAL,
												ports		=> et_symbol_ports.pac_ports.empty_map, -- ports will come later
												-- NOTE: Other placeholders discarded here.
												
												others		=> <>)
										));

						end case;
								
						-- copy ports 
						pac_units_internal.update_element (
							container	=> device.units_internal,
							position	=> unit_cursor,
							process		=> copy_ports'access);
						
						next (unit_cursor_kicad);

						log_indentation_down;
					end loop;
				end copy_units;

				

				-- The package associated with a variant must be changed so that it becomes 
				-- something like libraries/packages/__-__-lbr-transistors.pretty_S_0805.pac
				procedure rename_package_model_in_variants (
					device_name	: in pac_device_model_file.bounded_string; -- libraries/devices/transistors/pnp.dev
					device		: in out type_device_model) 
				is
					use et_package_variant;
					use pac_package_variant_name;
					use pac_variants;
					
					variant_cursor : pac_variants.cursor := device.variants.first;

					
					procedure rename (
						variant_name	: in pac_package_variant_name.bounded_string; -- N, D, ...
						variant			: in out type_variant)
					is begin
						variant.package_model := (rename_package_model (variant.package_model)); -- ../../lbr/transistors.pretty/S_0805

						log (text => "package variant " & to_string (variant_name) 
							 & " now uses package " 
							 & to_string (name => variant.package_model), level => log_threshold + 4);
					end rename;
					
					
				begin
					-- Loop in variants and rename the package names.
					while variant_cursor /= pac_variants.no_element loop

						pac_variants.update_element (
							container	=> device.variants,
							position	=> variant_cursor,
							process		=> rename'access);
						
						next (variant_cursor);
					end loop;
				end rename_package_model_in_variants;

				
				-- Removes the leading hash character from the prefix of a virtual component like #FLG or #PWR.
				function remove_leading_hash (
					prefix : in pac_device_prefix.bounded_string) return
					pac_device_prefix.bounded_string 
				is
					use pac_device_prefix;
				begin
					return to_prefix (slice (prefix, 2, length (prefix))); -- FLG, PWR
				end;

				
			begin -- query_components
				while component_cursor /= et_kicad_libraries.type_components_library.no_element loop
					generic_name := et_kicad_libraries.strip_tilde (key (component_cursor));
					--log (text => "device " & to_string (generic_name), level => log_threshold + 2);

					-- Build the name of the device model from the component library name and generic name:
					device_model := concatenate_lib_name_and_generic_name (component_library_name, generic_name); -- ../lbr/logic_ttl/7400.dev

					-- Create a new device model in container et_libraries.devices:
					log (text => "device model " & to_string (name => device_model), level => log_threshold + 3);
					log_indentation_up;

					case element (component_cursor).appearance is
						when APPEARANCE_VIRTUAL =>
							pac_devices_lib.insert (
								container	=> device_library,
								position	=> device_cursor,
								inserted	=> inserted,
								key			=> device_model,
								new_item	=> (
									appearance		=> APPEARANCE_VIRTUAL,
									prefix 			=> remove_leading_hash (element (component_cursor).prefix),
									units_internal	=> <>, -- internal units will come later
									units_external	=> <> -- kicad components do not have external symbols
									-- NOTE: Kicad value of power symbols is discarded.
									-- NOTE: KiCad power_flag is discarded.
								));

						when APPEARANCE_PCB =>
							--log (text => "variant count " & count_type'image (pac_variants.length (element (component_cursor).variants)));
							
							pac_devices_lib.insert (
								container	=> device_library,
								position	=> device_cursor,
								inserted	=> inserted,
								key			=> device_model,
								new_item	=> (
									appearance		=> APPEARANCE_PCB,
									prefix 			=> element (component_cursor).prefix,
									value			=> element (component_cursor).value,
									units_internal	=> <>, -- internal units will come later
									units_external	=> <>, -- kicad components do not have external symbols

									-- kicad does not provide a partcode. so we assign the default partcode
									--partcode		=> et_libraries.to_partcode (et_libraries.partcode_default), 
									variants		=> element (component_cursor).variants

									-- NOTE: KiCad package_filter and datasheet are discarded.
								));

								-- If multiple designs are converted, a particular device might be 
								-- in et_libraries.devices already. The flag "inserted" would be false 
								-- in this case. Renaming the package in the variants must not take place
								-- again because it had been done in ealier project conversions:
								if inserted then
									-- rename package model file name in variants
									pac_devices_lib.update_element (
										container	=> device_library,
										position	=> device_cursor,
										process		=> rename_package_model_in_variants'access);
								end if;
									
					end case;

					-- If multiple designs are converted, a particular device might be 
					-- in et_libraries.devices already. The flag "inserted" would be false 
					-- in this case. Copying units must not take place again:
					if inserted then
						-- Copy units.
						pac_devices_lib.update_element (
							container	=> device_library,
							position	=> device_cursor,
							process		=> copy_units'access);
					else
						log (text => "already there -> skipped", level => log_threshold + 3);
					end if;
					
					log_indentation_down;
					
					next (component_cursor);
				end loop;
			end query_components;


			
			-- Creates with the library name and package name new native package models.
			procedure query_packages (
				library_name	: in pac_package_model_file_name.bounded_string; -- projects/lbr/smd_packages.pretty
				library			: in et_kicad_packages.type_packages_library.map) 
			is

				use et_kicad_packages.type_packages_library;
				package_cursor_kicad	: et_kicad_packages.type_packages_library.cursor := library.first;
				package_name			: pac_package_name.bounded_string;
				package_model			: pac_package_model_file_name.bounded_string := library_name; -- projects/lbr/smd_packages.pretty

				use et_packages.pac_package_models;
				package_cursor			: et_packages.pac_package_models.cursor;
				inserted				: boolean;
			begin
				-- Loop in kicad packages (footprints) of the current library.
				while package_cursor_kicad /= et_kicad_packages.type_packages_library.no_element loop
					package_name := key (package_cursor_kicad); -- S_0805
					--log (text => "package name " & et_libraries.to_string (package_name), level => log_threshold + 2);

					-- build the new native package model name
					package_model := to_file_name (compose (
								containing_directory	=> to_string (name => library_name), -- projects/lbr/smd_packages.pretty
								name					=> to_string (packge => package_name))); -- S_0805

					-- replace . and / in package_model 
					package_model := rename_package_model (package_model);
					log (text => "package model " & to_string (name => package_model), level => log_threshold + 3);

					-- Insert the new package model in et_pcb.packages. In case the package is already in the 
					-- container (due to other project imports), the flag "inserted" will go false. The package
					-- would not be inserted again:
					et_packages.pac_package_models.insert (
						container	=> et_packages.package_models,
						key			=> package_model, -- libraries/packages/-home-user-lbr-bel_battery_pretty-S_CR3232.pac
						position	=> package_cursor,
						inserted	=> inserted,
						new_item	=> (et_packages.type_package_base (element (package_cursor_kicad)) with 
							silkscreen	=> element (package_cursor_kicad).silk_screen,
							assy_doc	=> element (package_cursor_kicad).assembly_documentation,
							terminals 	=> element (package_cursor_kicad).terminals)
							-- NOTE: The kicad timestamp is discarded here.
						);
					
					next (package_cursor_kicad);
				end loop;
			end query_packages;

			
		begin -- copy_libraries

			-- Loop in kicad component libraries:
			while component_library_cursor /= et_kicad_libraries.type_device_libraries.no_element loop
				component_library_name := key (component_library_cursor);
				log (text => "component library " & to_string (name => component_library_name), level => log_threshold + 2);

				log_indentation_up;
				
				query_element (
					position	=> component_library_cursor,
					process		=> query_components'access);

				log_indentation_down;
				next (component_library_cursor);
			end loop;


			-- The way package libraries are copied/converted depends on the kicad format:
			case et_import.cad_format is

				when et_import.KICAD_V4 =>

					-- With kicad v4 the package libraries live in a global container et_kicad_pcb.package_libraries.
					-- Thus, the conversion is a one-time operation. It does not need to be performed for each module 
					-- over and over.
					if not packages_v4_copied then
						package_library_cursor := et_kicad_packages.package_libraries.first;

						-- Loop in footprint libraries:
						while package_library_cursor /= et_kicad_packages.type_libraries.no_element loop
							log (text => "package library " & to_string (name => key (package_library_cursor)), level => log_threshold + 2);

							log_indentation_up;

							query_element (
								position	=> package_library_cursor,
								process		=> query_packages'access);
							
							log_indentation_down;
							next (package_library_cursor);
						end loop;

						packages_v4_copied := true; -- When processing the next module, there is no need for copying again.
					end if;

					
				-- Kicad V5 package libraries are module specific (selector "footprints") and are converted here:
				when et_import.KICAD_V5 =>

					-- Loop in footprint libraries:
					while package_library_cursor /= et_kicad_packages.type_libraries.no_element loop
						log (text => "package library " & to_string (name => key (package_library_cursor)), level => log_threshold + 2);

						log_indentation_up;

						query_element (
							position	=> package_library_cursor,
							process		=> query_packages'access);
						
						log_indentation_down;
						next (package_library_cursor);
					end loop;

				when others => null;
			end case;
			
		end copy_libraries;


		
		-- Saves the library containers in the current working directory.
		procedure save_libraries (
			log_threshold	: in type_log_level) 
		is
			use et_string_processing;
			use pac_devices_lib;

			
			procedure save_device (device_cursor : in pac_devices_lib.cursor) is
			begin
				et_device_rw.save_device (
					-- library name like: 
					-- libraries/devices/__-__-lbr-bel_connector_and_jumper_FEMALE_01X06.dev
					file_name		=> to_file_name (to_string (key (device_cursor))),

					-- the device model itself:
					device			=> element (device_cursor),
					log_threshold	=> log_threshold + 1); 
			end save_device;

			
			use et_packages.pac_package_models;

			
			procedure save_package (package_cursor : in et_packages.pac_package_models.cursor) is
			begin
				et_pcb_rw.device_packages.save_package (
					-- package name like: 
					-- libraries/packages/__-__-lbr-bel_connector_and_jumper_FEMALE_01X06.pac
					file_name		=> to_file_name (name => to_string (key (package_cursor))),

					-- the package model itself:
					packge			=> element (package_cursor),
					log_threshold	=> log_threshold + 1); 
			end save_package;

			
		begin -- save_libraries
			log (text => "saving libraries ...", level => log_threshold);
			log_indentation_up;

			log (text => "devices (former KiCad components) ...", level => log_threshold + 1);
			log_indentation_up;
			iterate (device_library, save_device'access);
			log_indentation_down;
			
			log (text => "packages (former KiCad footprints) ...", level => log_threshold + 1);
			log_indentation_up;
			iterate (et_packages.package_models, save_package'access);
			log_indentation_down;

			log_indentation_down;			
		end save_libraries;



	begin -- to_native
	
		-- First, the kicad designs (currently there is only one) must be flattened so that we get real flat designs.
		-- Further-on the y coordinates of objects in schematics and layouts must be changed. 
		-- Kicad schematic has origin in upper left corner. ET has origin in lower left corder.
		transpose (log_threshold);

		log (text => "converting ...", level => log_threshold);
		log_indentation_up;

		-- Now we copy content from the kicad modules to the same named native modules.
		-- CS: currently there is only one kicad and only one native module.
		-- So this loop will be executed only once:
		while module_cursor_kicad /= et_kicad.pcb.type_modules.no_element loop

			-- For each kicad design we create a native project.
			et_project.create_project_directory (
				project_name	=> project_name, -- blood_sample_analyzer
				log_threshold 	=> log_threshold + 2);
			
			-- Clear scratch module because in the following everything goes there.
			module := (others => <>);
			
			log_indentation_up;
			
			copy_general_stuff;
			copy_components;
			copy_nets;			
			copy_frames; -- CS: not completed yet -- no longer required. remove ?

			-- Copy component libraries.
			-- NOTE: In Kicad component libraries are always project dependend.
			-- Thus the libraries are taken from the kicad module
			-- and then copied into the global ET native library containers:
			--  et_libraries.devices
			--  et_pcb.packages
			-- Inside the ET native libraries the former Kicad libraries are
			-- named after their location in the file system (sym-lib-tables and fp-lib-tables).
			query_element (
				position	=> module_cursor_kicad,
				process		=> copy_libraries'access);

			-- Since procedure save_module requires a cursor to the module we set up a list
			-- that contains only the scratch module.
			-- CS: It would be more efficient if the scratch module were in such a list from the
			-- beginning and procedures copy_general_stuff, copy_components, copy_nets, copy_frames
			-- and copy_libraries would update the scratch module inside the list.
			declare 
				current_working_directory : constant string := current_directory;
				use pac_generic_modules;
				module_list : pac_generic_modules.map; -- set up the list
				use et_module_ops;
			begin
				-- insert the scratch module in the list
				insert (
					container 	=> module_list,
					key			=> to_module_name (to_string (project_name)), -- blood_sample_analyzer
					new_item	=> module); -- the native generic scratch module

				set_directory (to_string (project_name));
				
				-- save module (the first an only one in module_list) in file *.mod
				log (text => "saving module " & enclose_in_quotes (to_string (project_name)),
					 level => log_threshold + 3);
				
				save_module (
					module_cursor	=> module_list.first,
					log_threshold	=> log_threshold);

				-- save libraries in native project in sub-directories:
				-- libraries/devices and libraries/packages
				save_libraries (log_threshold + 1);
				
				set_directory (current_working_directory);
			end;
			
			log_indentation_down;
			next (module_cursor_kicad);
		end loop;


		
		log_indentation_down;

		exception
			when event: others =>

				-- output the line of code where the exception occured:
				show_line (file => gnat.source_info.file, line => gnat.source_info.line);
				raise;
		
	end to_native;
	

	
end et_kicad_to_native;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
