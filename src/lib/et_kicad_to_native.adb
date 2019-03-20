------------------------------------------------------------------------------
--                                                                          --
--                       SYSTEM ET KICAD_TO_NATIVE                          --
--                                                                          --
--                                 ET                                       --
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

--   For correct displaying set tab width in your edtior to 4.

--   The two letters "CS" indicate a "construction side" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--   ToDo:
--		1. Warning if virtual component pins apply for all units. Usually 
--			virtual components (such as power flags) have only one unit. If the
--			check "common to all units in component" is set, ET generates an
--			extra unit. Why ? ET assumes the affeced pin is a power pin. Power pins
--			in turn are assigned to an extra unit (in EAGLE we speak of "supply symbols").
--		2. Warning if virtual component with one power pin has pin direction differing from power_out
--			Example: Power symbol "P3V3" must have pin direction power_out.	
--		3. Make sure ports of netchangers are named like 1 or 2.

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.directories;			use ada.directories;
with gnat.directory_operations;
with ada.exceptions; 			use ada.exceptions;
with gnat.source_info;

with et_coordinates;
with et_libraries;
with et_schematic;
with et_geometry;

with et_general;				use et_general;
with et_string_processing;		use et_string_processing;
with et_project;
with et_pcb;
with et_pcb_coordinates;
with et_kicad_general;
with et_kicad;
with kicad_coordinates;
with et_kicad_pcb;
with et_import;
with et_export;
with et_csv;

package body et_kicad_to_native is

	use et_general.type_net_name;
	
	procedure transpose (log_threshold : in et_string_processing.type_log_level) is
	-- Transposes coordinates of schematic and layout elements:
	-- 1. In schematic changes the path (selector of et_coordinates.type_coordinates) to the root path (/).
	--    CS: Native coordinates currently do not require the "path" selector. The change-path-stuff is thus not required.
	-- 2. Moves schematic and layout objects from negative to positive y coordinates.
	--    (The origin in kicad is the upper left corner. The origin in ET is the lower left corner.)
		use et_kicad.type_modules;
		module_cursor : et_kicad.type_modules.cursor := et_kicad.type_modules.first (et_kicad.modules);

		root : kicad_coordinates.type_path_to_submodule.list := kicad_coordinates.type_path_to_submodule.empty_list;
-- 		before	: constant string (1..15) := "position before";
-- 		now		: constant string (1..15) := "position now   ";
		before	: constant string (1..6) := "before";
		now		: constant string (1..6) := "now   ";

		-- This list of frames serves to map from sheet number to paper size:
		schematic_frames : et_kicad.type_frames.list;

		-- Here the height of the layout sheet is kept. It is required for move ops of 
		-- layout objects from the kicad frame to the native frame.
		layout_sheet_height : et_pcb_coordinates.type_distance;

		function board_available return boolean is
		-- Returns true if the current kicad module has a layout file.
		begin
			if element (module_cursor).board_available then
				return true;
			else 
				return false;
			end if;
		end board_available;
		
		function paper_size_of_schematic_sheet (sheet_number : in et_coordinates.type_sheet)
		-- Returns for a given sheet number the respective paper size.
			return et_general.type_paper_size is

			-- This is to be returned. In case no paper size was found, use the default value of type_paper_size.
			size : et_general.type_paper_size := et_general.type_paper_size'first;

			sheet_found : boolean := false; -- goes true once the given sheet has been found
		
			procedure query_sheet_number (frame : in et_kicad.type_frame) is
				use et_coordinates;
			begin
				if kicad_coordinates.sheet (frame.coordinates) = sheet_number then
					size := frame.paper_size;
					sheet_found := true;
				end if;
			end query_sheet_number;

			-- We search for the paper size in the list "frames":
			use et_kicad.type_frames;
			frame_cursor : et_kicad.type_frames.cursor := schematic_frames.first;
			
		begin -- paper_size_of_schematic_sheet

			-- loop in list of frames given in "frames"
			while frame_cursor /= et_kicad.type_frames.no_element loop
				
				query_element (
					position	=> frame_cursor,
					process		=> query_sheet_number'access);

				if sheet_found then exit; end if; -- cancel search once the given sheet has been found
				
				next (frame_cursor);
			end loop;

			if not sheet_found then
				log_indentation_reset;
				log (message_error & "sheet with number" & et_coordinates.to_sheet (sheet_number) & " not found !");
				raise constraint_error;
			end if;
			
			return size;
		end paper_size_of_schematic_sheet;

		procedure move (point : in out kicad_coordinates.type_coordinates) is
		-- Transposes a schematic point from the kicad frame to the ET native frame.
		-- KiCad frames have the origin in the upper left corner.
		-- ET frames have the origin in the lower left corner.
			use et_coordinates;
			sheet_number 		: et_coordinates.type_sheet;
			sheet_paper_size	: et_general.type_paper_size;
			sheet_height		: et_coordinates.type_distance_xy;
			new_y				: et_coordinates.type_distance_xy;
		begin -- move
			-- get the sheet number where the given point resides
			sheet_number		:= kicad_coordinates.sheet (point); 

			-- get the paper size of the sheet
			sheet_paper_size	:= paper_size_of_schematic_sheet (sheet_number);

			-- get the paper height of the sheet
			sheet_height		:= paper_dimension (axis => Y, paper_size => sheet_paper_size);

			-- calculate the new y position
			new_y				:= sheet_height - distance (axis => Y, point => point);

			-- assign the new y position to the given point
			set_y (point, new_y);
		end move;

		procedure move (
			point_actual	: in out et_coordinates.type_point;	-- the point it is about
			point_help		: in kicad_coordinates.type_coordinates	-- supportive point that proviedes the sheet number
			) is
		-- Transposes the schematic point_actual from the kicad frame to the ET native frame.
		-- point_help has supporting purpose: it provides the sheet number where point_actual sits.
		-- KiCad frames have the origin in the upper left corner.
		-- ET frames have the origin in the lower left corner.
			use et_coordinates;
			sheet_number 		: et_coordinates.type_sheet;
			sheet_paper_size	: et_general.type_paper_size;
			sheet_height		: et_coordinates.type_distance_xy;
			new_y				: et_coordinates.type_distance_xy;
		begin -- move
			-- get the sheet number where the given point resides
			sheet_number		:= kicad_coordinates.sheet (point_help); 

			-- get the paper size of the sheet
			sheet_paper_size	:= paper_size_of_schematic_sheet (sheet_number);

			-- get the paper height of the sheet
			sheet_height		:= paper_dimension (axis => Y, paper_size => sheet_paper_size);

			-- calculate the new y position
			--new_y				:= sheet_height - distance_y (point_actual);
			new_y				:= sheet_height - distance (axis => Y, point => point_actual);			

			-- assign the new y position to the given point
			set_y (point_actual, new_y);
		end move;

		procedure prepare_layout_y_movements is
		-- Sets the layout_sheet_height depending on the paper size of the layout sheet.
			-- The paper size of a board/layout drawing:
			use et_pcb_coordinates;
			board_paper_size : et_general.type_paper_size;
		begin -- prepare_layout_y_movements
			-- Fetch the paper size of the current layout design:
			board_paper_size := element (module_cursor).board.paper_size;
			
			log ("layout paper size " & et_general.to_string (board_paper_size), log_threshold + 2);
			
			-- get the paper height of the sheet
			layout_sheet_height := paper_dimension (axis => Y, paper_size => board_paper_size);
		end prepare_layout_y_movements;

		procedure move (point : in out et_pcb_coordinates.type_point_2d'class) is
		-- Transposes the given point in layout from the kicad frame to the ET native frame.
		-- KiCad frames have the origin in the upper left corner.
		-- ET frames have the origin in the lower left corner.
			use et_pcb_coordinates;
			new_y : et_pcb_coordinates.type_distance;
		begin -- move
			new_y := layout_sheet_height - get_axis (Y, point);
			set_point (Y, new_y, point);
		end move;
		
		procedure flatten_notes (
			module_name	: in kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is
		-- Changes the path and y position of text notes (in schematic).

			use et_kicad.type_texts;
			note_cursor : et_kicad.type_texts.cursor := module.notes.first;

			procedure change_path (note : in out et_kicad.type_text) is
				use et_coordinates;
				use kicad_coordinates;
			begin
				log ("note '" & et_libraries.to_string (note.content) & "'", log_threshold + 3);
				log_indentation_up;
				
				log (before & et_coordinates.to_string (note.coordinates), log_threshold + 4);

				-- Move position from negative to positive y.
				move (note.coordinates);

				log (now & et_coordinates.to_string (note.coordinates), log_threshold + 4);

				log_indentation_down;
			end change_path;
				
		begin -- flatten_notes
			log ("text notes ...", log_threshold + 2);
			log_indentation_up;
			
			while note_cursor /= et_kicad.type_texts.no_element loop
				et_kicad.type_texts.update_element (
					container	=> module.notes,
					position	=> note_cursor,
					process		=> change_path'access);

				next (note_cursor);
			end loop;

			log_indentation_down;
		end flatten_notes;

		procedure flatten_frames (
			module_name	: in kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is
		-- Changes the path of drawing frames (in schematic) to root path.
			
			use et_kicad.type_frames;
			frame_cursor : et_kicad.type_frames.cursor := module.frames.first;

			procedure change_path (frame : in out et_kicad.type_frame) is
				use et_coordinates;
			begin
				-- CS what should be logged here ?
				log_indentation_up;
				
				log (before & kicad_coordinates.to_string (position => frame.coordinates, scope => kicad_coordinates.MODULE),
					 log_threshold + 4);

				kicad_coordinates.set_path (frame.coordinates, root);

				log (now & kicad_coordinates.to_string (position => frame.coordinates, scope => kicad_coordinates.MODULE),
					 log_threshold + 4);

				log_indentation_down;
			end change_path;
				
		begin -- flatten_frames
			log ("frames ...", log_threshold + 2);
			log_indentation_up;
			
			while frame_cursor /= et_kicad.type_frames.no_element loop
				et_kicad.type_frames.update_element (
					container	=> module.frames,
					position	=> frame_cursor,
					process		=> change_path'access);

				next (frame_cursor);
			end loop;

			log_indentation_down;
		end flatten_frames;

		procedure flatten_components (
			module_name	: in kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is
		-- Changes the path and y position of units of components (in schematic) to root path.
		-- Moves the y position of components (in layout).
			
			use et_kicad.type_components_schematic;
			component_cursor : et_kicad.type_components_schematic.cursor := module.components.first;

			procedure query_units (
				reference	: in et_libraries.type_device_name;
				component	: in out et_kicad.type_component_schematic) is
				use et_coordinates;
				use et_kicad.type_units_schematic;
				unit_cursor : et_kicad.type_units_schematic.cursor := component.units.first;

				procedure change_path (
					unit_name	: in et_libraries.type_unit_name.bounded_string;
					unit		: in out et_kicad.type_unit_schematic) is
					use et_coordinates;
				begin
					log ("unit " & et_libraries.to_string (unit_name), log_threshold + 4);
					log_indentation_up;
					
					log (before & kicad_coordinates.to_string (position => unit.position, scope => kicad_coordinates.MODULE),
						log_threshold + 4);

					kicad_coordinates.set_path (unit.position, root);

					move (unit.position); -- Move position from negative to positive y.

					log (now & kicad_coordinates.to_string (position => unit.position, scope => kicad_coordinates.MODULE),
						log_threshold + 4);

					log_indentation_down;
				end change_path;

				procedure move_package is
				-- moves the position of the package in layout
					use et_libraries;
					use et_pcb_coordinates;
				begin
					if component.appearance = SCH_PCB then
						log_indentation_up;
						
						log ("package", log_threshold + 4);
						
						log_indentation_up;
						log (before & to_string (type_point_2d (component.position)), log_threshold + 4);
						move (point => component.position);
						log (now & to_string (type_point_2d (component.position)), log_threshold + 4);
						
						log_indentation_down;
						log_indentation_down;
					end if;
				end move_package;
				
			begin -- query_units
				log (et_libraries.to_string (key (component_cursor)), log_threshold + 3);
				log_indentation_up;

				while unit_cursor /= et_kicad.type_units_schematic.no_element loop
					
					et_kicad.type_units_schematic.update_element (
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
			log ("components ...", log_threshold + 2);
			log_indentation_up;
			
			while component_cursor /= et_kicad.type_components_schematic.no_element loop

				et_kicad.type_components_schematic.update_element (
					container	=> module.components,
					position	=> component_cursor,
					process		=> query_units'access);

				next (component_cursor);
			end loop;

			log_indentation_down;
		end flatten_components;

		procedure flatten_nets (
			module_name	: in kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is
		-- Changes the path and y position of net segments, junctions and labels (in schematic) to root path.
		-- MOves the y position of copper objects (in layout).

			use et_kicad.type_nets;
			net_cursor : et_kicad.type_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in type_net_name.bounded_string;
				net			: in out et_kicad.type_net) is

				use et_kicad.type_strands;
				strand_cursor : et_kicad.type_strands.cursor := net.strands.first;

				procedure query_segments (strand : in out et_kicad.type_strand) is
					use et_kicad.type_net_segments;
					segment_cursor : et_kicad.type_net_segments.cursor := strand.segments.first;

					procedure change_path_of_segment (segment : in out et_kicad.type_net_segment) is
						use et_coordinates;
						
						use et_kicad.type_simple_labels;
						simple_label_cursor : et_kicad.type_simple_labels.cursor := segment.label_list_simple.first;

						procedure move_simple_label (label : in out et_kicad.type_net_label_simple) is
						-- Moves the given simple label from kicad frame to native frame.
						begin
							log ("simple label " & before & to_string (label.coordinates), log_threshold + 3);
							
							-- As supportive point that provides the sheet number we pass the segment start position.
							-- coordinates of net labels do not posess a sheet number.
							move (point_actual => label.coordinates, point_help => segment.coordinates_start);

							log ("simple label " & now & to_string (label.coordinates), log_threshold + 3);							
						end move_simple_label;
						
						use et_kicad.type_tag_labels;
						tag_label_cursor : et_kicad.type_tag_labels.cursor := segment.label_list_tag.first;

						procedure move_tag_label (label : in out et_kicad.type_net_label_tag) is
						-- Moves the given tag label from kicad frame to native frame.
						begin
							log ("tag label " & before & to_string (label.coordinates), log_threshold + 3);
							
							-- As supportive point that provides the sheet number we pass the segment start position.
							-- coordinates of net labels do not posess a sheet number.
							move (point_actual => label.coordinates, point_help => segment.coordinates_start);
							
							log ("tag label " & now & to_string (label.coordinates), log_threshold + 3);
						end move_tag_label;

						use et_kicad.type_junctions;
						junction_cursor : et_kicad.type_junctions.cursor := segment.junctions.first;

						procedure change_path_of_junction (junction : in out et_kicad.type_net_junction) is
						-- Moves the given net junction from kicad frame to native frame.
							use kicad_coordinates;
						begin
							log ("junction " & before & kicad_coordinates.to_string (
								position => junction.coordinates, scope => kicad_coordinates.MODULE),
								log_threshold + 3);

							set_path (junction.coordinates, root);
							move (junction.coordinates);

							log ("junction " & now & to_string (
								position => junction.coordinates, scope => kicad_coordinates.MODULE),
								log_threshold + 3);
								 
						end change_path_of_junction;
								 
					begin -- change_path_of_segment
						log ("schematic net segment", log_threshold + 3);
						log_indentation_up;

						-- start point of net segment
						log ("start " & before & kicad_coordinates.to_string (
							position => segment.coordinates_start, scope => kicad_coordinates.MODULE),
							log_threshold + 3);

						kicad_coordinates.set_path (segment.coordinates_start, root);

						move (segment.coordinates_start); -- Move position from negative to positive y.
						
						log ("start " & now & kicad_coordinates.to_string (
							position => segment.coordinates_start, scope => kicad_coordinates.MODULE),
							log_threshold + 3);

						-- end point of net segment
						log ("end   " & before & kicad_coordinates.to_string (
							position => segment.coordinates_end, scope => kicad_coordinates.MODULE),
							log_threshold + 3);

						kicad_coordinates.set_path (segment.coordinates_end, root);

						move (segment.coordinates_end); -- Move position from negative to positive y.
						
						log ("end   " & now & kicad_coordinates.to_string (
							position => segment.coordinates_end, scope => kicad_coordinates.MODULE),
							log_threshold + 3);

						-- Move y of simple net labels.
						while simple_label_cursor /= et_kicad.type_simple_labels.no_element loop
							et_kicad.type_simple_labels.update_element (
								container	=> segment.label_list_simple,
								position	=> simple_label_cursor,
								process 	=> move_simple_label'access);
							next (simple_label_cursor);
						end loop;

						-- Move y of tag net labels.
						while tag_label_cursor /= et_kicad.type_tag_labels.no_element loop
							et_kicad.type_tag_labels.update_element (
								container	=> segment.label_list_tag,
								position	=> tag_label_cursor,
								process 	=> move_tag_label'access);
							next (tag_label_cursor);
						end loop;

						-- Change path of junctions (incl. moving y):
						while junction_cursor /= et_kicad.type_junctions.no_element loop
							et_kicad.type_junctions.update_element (
								container	=> segment.junctions,
								position	=> junction_cursor,
								process 	=> change_path_of_junction'access);
							next (junction_cursor);
						end loop;
						
						log_indentation_down;

					end change_path_of_segment;
					
				begin -- query_segments

					-- Move the start coordinates of the strand from kicad frame to native frame:
					log ("schematic strand start " & before & et_coordinates.to_string (point => strand.position), log_threshold + 3);
					move (strand.position); 
					log ("schematic strand start " & now & et_coordinates.to_string (point => strand.position), log_threshold + 3);

					-- Change path of segments:
					while segment_cursor /= et_kicad.type_net_segments.no_element loop

						et_kicad.type_net_segments.update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> change_path_of_segment'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;

				procedure move_route is
				-- Move y position of copper objects of the net: lines, arcs, vias, polygons

					use et_pcb.type_copper_lines_pcb;
					use et_pcb.type_copper_arcs_pcb;
					use et_pcb.type_vias;
					use et_pcb.type_copper_polygons_signal;
					
					line_cursor : et_pcb.type_copper_lines_pcb.cursor := net.route.lines.first;
					arc_cursor	: et_pcb.type_copper_arcs_pcb.cursor := net.route.arcs.first;
					via_cursor	: et_pcb.type_vias.cursor := net.route.vias.first;
					poly_cursor	: et_pcb.type_copper_polygons_signal.cursor := net.route.polygons.first;

					board_track : constant string (1..12) := "board track ";
					
					procedure move_line (line : in out et_pcb.type_copper_line_pcb) is
						use et_pcb;
					begin
						log (board_track & "line", log_threshold + 4);
						log_indentation_up;

						log (before & to_string (line), log_threshold + 4);

						move (line.start_point);
						move (line.end_point);
						
						log (now & to_string (line), log_threshold + 4);
						
						log_indentation_down;
					end move_line;

					procedure move_arc (arc : in out et_pcb.type_copper_arc_pcb) is
						use et_pcb;
					begin
						log (board_track & "arc", log_threshold + 4);
						log_indentation_up;

						log (before & to_string (arc), log_threshold + 4);

						move (arc.center);
						move (arc.start_point);
						move (arc.end_point);

						log (now & to_string (arc), log_threshold + 4);
						
						log_indentation_down;
					end move_arc;

					procedure move_via (via : in out et_pcb.type_via) is
						use et_pcb_coordinates;
					begin
						log (board_track & "via", log_threshold + 4);
						log_indentation_up;

						log (before & to_string (via.position), log_threshold + 4);

						move (via.position);

						log (now & to_string (via.position), log_threshold + 4);
						
						log_indentation_down;
					end move_via;

					procedure move_polygon (polygon : in out et_pcb.type_copper_polygon_signal) is
						use et_pcb_coordinates;
						use et_pcb.type_polygon_points;
						point_cursor : et_pcb.type_polygon_points.cursor := polygon.corners.first;

						new_points : et_pcb.type_polygon_points.set;
						
						procedure get_point (point : in type_point_2d) is
						-- Reads a corner point, copies it, moves the copy and inserts the moved
						-- copy in a new set "new_points".
							new_point : type_point_2d := point; -- copy given point
						begin
							log (before & to_string (new_point), log_threshold + 4);
							move (new_point); -- move copied point
							log (now & to_string (new_point), log_threshold + 4);

							-- insert new point in new_points:
							et_pcb.type_polygon_points.insert (
								container	=> new_points,
								new_item	=> new_point);
							
						end get_point;
						
					begin -- move_polygon
						log ("board polygon corner points", log_threshold + 4);
						log_indentation_up;

						-- loop through polygon corner points and read one after another:
						while point_cursor /= et_pcb.type_polygon_points.no_element loop

							et_pcb.type_polygon_points.query_element (
								position	=> point_cursor,
								process		=> get_point'access);
							
							next (point_cursor);
						end loop;

						-- Now the new set of polygon corner points is available in "new_points".
						-- new_points replaces the old list of points:
						polygon.corners := new_points;
						
						log_indentation_down;
					end move_polygon;
					
				begin -- move_route
					
					-- Move lines:
					while line_cursor /= et_pcb.type_copper_lines_pcb.no_element loop
						et_pcb.type_copper_lines_pcb.update_element (
							container 	=> net.route.lines,
							position	=> line_cursor,
							process		=> move_line'access);
						
						next (line_cursor);
					end loop;

					-- Move arcs:
					while arc_cursor /= et_pcb.type_copper_arcs_pcb.no_element loop
						et_pcb.type_copper_arcs_pcb.update_element (
							container 	=> net.route.arcs,
							position	=> arc_cursor,
							process		=> move_arc'access);

						next (arc_cursor);
					end loop;

					-- Move vias:
					while via_cursor /= et_pcb.type_vias.no_element loop
						et_pcb.type_vias.update_element (
							container 	=> net.route.vias,
							position	=> via_cursor,
							process		=> move_via'access);

						next (via_cursor);
					end loop;

					while poly_cursor /= et_pcb.type_copper_polygons_signal.no_element loop
						et_pcb.type_copper_polygons_signal.update_element (
							container 	=> net.route.polygons,
							position	=> poly_cursor,
							process		=> move_polygon'access);

						next (poly_cursor);
					end loop;
					
				end move_route;
				
			begin -- query_strands

				-- schematic related:
				while strand_cursor /= et_kicad.type_strands.no_element loop

					et_kicad.type_strands.update_element (
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
			log ("nets ...", log_threshold + 2);
			log_indentation_up;
			
			while net_cursor /= et_kicad.type_nets.no_element loop
				log (et_general.to_string (key (net_cursor)), log_threshold + 3);

				log_indentation_up;
				
				et_kicad.type_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor,
					process		=> query_strands'access);

				log_indentation_down;
				
				next (net_cursor);
			end loop;

			log_indentation_down;
		end flatten_nets;

		procedure move_general_board_stuff (
		-- Moves y positon of general (non-component related) layout objects from kicad frame to native frame.
			module_name	: in kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is

			log_threshold_add : type_log_level := 2;
			
			procedure move_silk_screen is
				use et_pcb.type_silk_lines;
				lines_cursor : et_pcb.type_silk_lines.cursor;

				use et_pcb.type_silk_arcs;
				arcs_cursor : et_pcb.type_silk_arcs.cursor;

				use et_pcb.type_silk_circles;
				circles_cursor : et_pcb.type_silk_circles.cursor;

				use et_pcb.type_silk_polygons;
				polygons_cursor : et_pcb.type_silk_polygons.cursor;

				use et_pcb.type_texts_with_content;
				texts_cursor : et_pcb.type_texts_with_content.cursor;
				
				board_silk_screen : constant string (1..18) := "board silk screen ";
				
				procedure move_line (line : in out et_pcb.type_silk_line) is
					use et_pcb;
				begin
					log (board_silk_screen & "line", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (line), log_threshold + log_threshold_add);

					move (line.start_point);
					move (line.end_point);
					
					log (now & to_string (line), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				procedure move_arc (arc : in out et_pcb.type_silk_arc) is
					use et_pcb;
				begin
					log (board_silk_screen & "arc", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (arc), log_threshold + log_threshold_add);

					move (arc.center);
					move (arc.start_point);
					move (arc.end_point);
					
					log (now & to_string (arc), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				procedure move_circle (circle : in out et_pcb.type_silk_circle) is
					use et_pcb_coordinates;
				begin
					log (board_silk_screen & "circle", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & " center" & to_string (circle.center), log_threshold + log_threshold_add);

					move (circle.center);
					
					log (now & " center" & to_string (circle.center), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

				procedure move_polygon (polygon : in out et_pcb.type_silk_polygon) is
					use et_pcb_coordinates;
					use et_pcb.type_polygon_points;
					point_cursor : et_pcb.type_polygon_points.cursor := polygon.corners.first;
					new_points : et_pcb.type_polygon_points.set;

					procedure get_point (point : in type_point_2d) is
					-- Reads a corner point, copies it, moves the copy and inserts the moved
					-- copy in a new set "new_points".
						new_point : type_point_2d := point; -- copy given point
					begin
						log (before & to_string (new_point), log_threshold + log_threshold_add);
						move (new_point); -- move copied point
						log (now & to_string (new_point), log_threshold + log_threshold_add);

						-- insert new point in new_points:
						et_pcb.type_polygon_points.insert (
							container	=> new_points,
							new_item	=> new_point);
						
					end get_point;
					
				begin -- move_polygon
					log (board_silk_screen & "polygon corner points", log_threshold + log_threshold_add);
					log_indentation_up;

					-- loop through polygon corner points and read one after another:
					while point_cursor /= et_pcb.type_polygon_points.no_element loop

						et_pcb.type_polygon_points.query_element (
							position	=> point_cursor,
							process		=> get_point'access);
						
						next (point_cursor);
					end loop;

					-- Now the new set of polygon corner points is available in "new_points".
					-- new_points replaces the old list of points:
					polygon.corners := new_points;
					
					log_indentation_down;
				end move_polygon;

				procedure move_text (text : in out et_pcb.type_text_with_content) is
					use et_pcb_coordinates;
				begin
					log (board_silk_screen & "text", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (text.position), log_threshold + log_threshold_add);

					move (text.position);
					
					log (now & to_string (text.position), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_text;


			begin -- move_silk_screen
				
				-- LINES TOP
				lines_cursor := module.board.silk_screen.top.lines.first;
				while lines_cursor /= et_pcb.type_silk_lines.no_element loop
					et_pcb.type_silk_lines.update_element (
						container	=> module.board.silk_screen.top.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- LINES BOTTOM
				lines_cursor := module.board.silk_screen.bottom.lines.first;
				while lines_cursor /= et_pcb.type_silk_lines.no_element loop
					et_pcb.type_silk_lines.update_element (
						container	=> module.board.silk_screen.bottom.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- ARCS TOP
				arcs_cursor := module.board.silk_screen.top.arcs.first;
				while arcs_cursor /= et_pcb.type_silk_arcs.no_element loop
					et_pcb.type_silk_arcs.update_element (
						container	=> module.board.silk_screen.top.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				-- ARCS BOTTOM
				arcs_cursor := module.board.silk_screen.bottom.arcs.first;
				while arcs_cursor /= et_pcb.type_silk_arcs.no_element loop
					et_pcb.type_silk_arcs.update_element (
						container	=> module.board.silk_screen.bottom.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;			

				-- CIRCLES TOP
				circles_cursor := module.board.silk_screen.top.circles.first;
				while circles_cursor /= et_pcb.type_silk_circles.no_element loop
					et_pcb.type_silk_circles.update_element (
						container	=> module.board.silk_screen.top.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- CIRCLES BOTTOM
				circles_cursor := module.board.silk_screen.bottom.circles.first;
				while circles_cursor /= et_pcb.type_silk_circles.no_element loop
					et_pcb.type_silk_circles.update_element (
						container	=> module.board.silk_screen.bottom.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- POLYGONS TOP
				polygons_cursor := module.board.silk_screen.top.polygons.first;
				while polygons_cursor /= et_pcb.type_silk_polygons.no_element loop
					et_pcb.type_silk_polygons.update_element (
						container	=> module.board.silk_screen.top.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				-- POLYGONS BOTTOM
				polygons_cursor := module.board.silk_screen.bottom.polygons.first;
				while polygons_cursor /= et_pcb.type_silk_polygons.no_element loop
					et_pcb.type_silk_polygons.update_element (
						container	=> module.board.silk_screen.bottom.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;	

				-- TEXTS TOP
				texts_cursor := module.board.silk_screen.top.texts.first;
				while texts_cursor /= et_pcb.type_texts_with_content.no_element loop
					et_pcb.type_texts_with_content.update_element (
						container	=> module.board.silk_screen.top.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;

				-- TEXTS BOTTOM
				texts_cursor := module.board.silk_screen.bottom.texts.first;
				while texts_cursor /= et_pcb.type_texts_with_content.no_element loop
					et_pcb.type_texts_with_content.update_element (
						container	=> module.board.silk_screen.bottom.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;

			end move_silk_screen;

			procedure move_assembly_documentation is
				use et_pcb.type_doc_lines;
				lines_cursor : et_pcb.type_doc_lines.cursor;

				use et_pcb.type_doc_arcs;
				arcs_cursor : et_pcb.type_doc_arcs.cursor;

				use et_pcb.type_doc_circles;
				circles_cursor : et_pcb.type_doc_circles.cursor;

				use et_pcb.type_doc_polygons;
				polygons_cursor : et_pcb.type_doc_polygons.cursor;

				use et_pcb.type_texts_with_content;
				texts_cursor : et_pcb.type_texts_with_content.cursor;
				
				doc : constant string (1..29) := "board assembly documentation ";
				
				procedure move_line (line : in out et_pcb.type_doc_line) is
					use et_pcb;
				begin
					log (doc & "line", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (line), log_threshold + log_threshold_add);

					move (line.start_point);
					move (line.end_point);
					
					log (now & to_string (line), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				procedure move_arc (arc : in out et_pcb.type_doc_arc) is
					use et_pcb;
				begin
					log (doc & "arc", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (arc), log_threshold + log_threshold_add);

					move (arc.center);
					move (arc.start_point);
					move (arc.end_point);
					
					log (now & to_string (arc), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				procedure move_circle (circle : in out et_pcb.type_doc_circle) is
					use et_pcb_coordinates;
				begin
					log (doc & "circle", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & " center" & to_string (circle.center), log_threshold + log_threshold_add);

					move (circle.center);
					
					log (now & " center" & to_string (circle.center), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

				procedure move_polygon (polygon : in out et_pcb.type_doc_polygon) is
					use et_pcb_coordinates;
					use et_pcb.type_polygon_points;
					point_cursor : et_pcb.type_polygon_points.cursor := polygon.corners.first;
					new_points : et_pcb.type_polygon_points.set;

					procedure get_point (point : in type_point_2d) is
					-- Reads a corner point, copies it, moves the copy and inserts the moved
					-- copy in a new set "new_points".
						new_point : type_point_2d := point; -- copy given point
					begin
						log (before & to_string (new_point), log_threshold + log_threshold_add);
						move (new_point); -- move copied point
						log (now & to_string (new_point), log_threshold + log_threshold_add);

						-- insert new point in new_points:
						et_pcb.type_polygon_points.insert (
							container	=> new_points,
							new_item	=> new_point);
						
					end get_point;
					
				begin -- move_polygon
					log (doc & "polygon corner points", log_threshold + log_threshold_add);
					log_indentation_up;

					-- loop through polygon corner points and read one after another:
					while point_cursor /= et_pcb.type_polygon_points.no_element loop

						et_pcb.type_polygon_points.query_element (
							position	=> point_cursor,
							process		=> get_point'access);
						
						next (point_cursor);
					end loop;

					-- Now the new set of polygon corner points is available in "new_points".
					-- new_points replaces the old list of points:
					polygon.corners := new_points;
					
					log_indentation_down;
				end move_polygon;

				procedure move_text (text : in out et_pcb.type_text_with_content) is
					use et_pcb_coordinates;
				begin
					log (doc & "text", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (text.position), log_threshold + log_threshold_add);

					move (text.position);
					
					log (now & to_string (text.position), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_text;


			begin -- move_assembly_documentation
				
				-- LINES TOP
				lines_cursor := module.board.assy_doc.top.lines.first;
				while lines_cursor /= et_pcb.type_doc_lines.no_element loop
					et_pcb.type_doc_lines.update_element (
						container	=> module.board.assy_doc.top.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- LINES BOTTOM
				lines_cursor := module.board.assy_doc.bottom.lines.first;
				while lines_cursor /= et_pcb.type_doc_lines.no_element loop
					et_pcb.type_doc_lines.update_element (
						container	=> module.board.assy_doc.bottom.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- ARCS TOP
				arcs_cursor := module.board.assy_doc.top.arcs.first;
				while arcs_cursor /= et_pcb.type_doc_arcs.no_element loop
					et_pcb.type_doc_arcs.update_element (
						container	=> module.board.assy_doc.top.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				-- ARCS BOTTOM
				arcs_cursor := module.board.assy_doc.bottom.arcs.first;
				while arcs_cursor /= et_pcb.type_doc_arcs.no_element loop
					et_pcb.type_doc_arcs.update_element (
						container	=> module.board.assy_doc.bottom.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;			

				-- CIRCLES TOP
				circles_cursor := module.board.assy_doc.top.circles.first;
				while circles_cursor /= et_pcb.type_doc_circles.no_element loop
					et_pcb.type_doc_circles.update_element (
						container	=> module.board.assy_doc.top.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- CIRCLES BOTTOM
				circles_cursor := module.board.assy_doc.bottom.circles.first;
				while circles_cursor /= et_pcb.type_doc_circles.no_element loop
					et_pcb.type_doc_circles.update_element (
						container	=> module.board.assy_doc.bottom.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- POLYGONS TOP
				polygons_cursor := module.board.assy_doc.top.polygons.first;
				while polygons_cursor /= et_pcb.type_doc_polygons.no_element loop
					et_pcb.type_doc_polygons.update_element (
						container	=> module.board.assy_doc.top.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				-- POLYGONS BOTTOM
				polygons_cursor := module.board.assy_doc.bottom.polygons.first;
				while polygons_cursor /= et_pcb.type_doc_polygons.no_element loop
					et_pcb.type_doc_polygons.update_element (
						container	=> module.board.assy_doc.bottom.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;	

				-- TEXTS TOP
				texts_cursor := module.board.assy_doc.top.texts.first;
				while texts_cursor /= et_pcb.type_texts_with_content.no_element loop
					et_pcb.type_texts_with_content.update_element (
						container	=> module.board.assy_doc.top.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;

				-- TEXTS BOTTOM
				texts_cursor := module.board.assy_doc.bottom.texts.first;
				while texts_cursor /= et_pcb.type_texts_with_content.no_element loop
					et_pcb.type_texts_with_content.update_element (
						container	=> module.board.assy_doc.bottom.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;
				
			end move_assembly_documentation;

			procedure move_stencil is
				use et_pcb.type_stencil_lines;
				lines_cursor : et_pcb.type_stencil_lines.cursor;

				use et_pcb.type_stencil_arcs;
				arcs_cursor : et_pcb.type_stencil_arcs.cursor;

				use et_pcb.type_stencil_circles;
				circles_cursor : et_pcb.type_stencil_circles.cursor;

				use et_pcb.type_stencil_polygons;
				polygons_cursor : et_pcb.type_stencil_polygons.cursor;

				stencil : constant string (1..14) := "board stencil ";
				
				procedure move_line (line : in out et_pcb.type_stencil_line) is
					use et_pcb;
				begin
					log (stencil & "line", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (line), log_threshold + log_threshold_add);

					move (line.start_point);
					move (line.end_point);
					
					log (now & to_string (line), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				procedure move_arc (arc : in out et_pcb.type_stencil_arc) is
					use et_pcb;
				begin
					log (stencil & "arc", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (arc), log_threshold + log_threshold_add);

					move (arc.center);
					move (arc.start_point);
					move (arc.end_point);
					
					log (now & to_string (arc), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				procedure move_circle (circle : in out et_pcb.type_stencil_circle) is
					use et_pcb_coordinates;
				begin
					log (stencil & "circle", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & " center" & to_string (circle.center), log_threshold + log_threshold_add);

					move (circle.center);
					
					log (now & " center" & to_string (circle.center), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

				procedure move_polygon (polygon : in out et_pcb.type_stencil_polygon) is
					use et_pcb_coordinates;
					use et_pcb.type_polygon_points;
					point_cursor : et_pcb.type_polygon_points.cursor := polygon.corners.first;
					new_points : et_pcb.type_polygon_points.set;

					procedure get_point (point : in type_point_2d) is
					-- Reads a corner point, copies it, moves the copy and inserts the moved
					-- copy in a new set "new_points".
						new_point : type_point_2d := point; -- copy given point
					begin
						log (before & to_string (new_point), log_threshold + log_threshold_add);
						move (new_point); -- move copied point
						log (now & to_string (new_point), log_threshold + log_threshold_add);

						-- insert new point in new_points:
						et_pcb.type_polygon_points.insert (
							container	=> new_points,
							new_item	=> new_point);
						
					end get_point;
					
				begin -- move_polygon
					log (stencil & "polygon corner points", log_threshold + log_threshold_add);
					log_indentation_up;

					-- loop through polygon corner points and read one after another:
					while point_cursor /= et_pcb.type_polygon_points.no_element loop

						et_pcb.type_polygon_points.query_element (
							position	=> point_cursor,
							process		=> get_point'access);
						
						next (point_cursor);
					end loop;

					-- Now the new set of polygon corner points is available in "new_points".
					-- new_points replaces the old list of points:
					polygon.corners := new_points;
					
					log_indentation_down;
				end move_polygon;

			begin -- move_stencil
				
				-- LINES TOP
				lines_cursor := module.board.stencil.top.lines.first;
				while lines_cursor /= et_pcb.type_stencil_lines.no_element loop
					et_pcb.type_stencil_lines.update_element (
						container	=> module.board.stencil.top.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- LINES BOTTOM
				lines_cursor := module.board.stencil.bottom.lines.first;
				while lines_cursor /= et_pcb.type_stencil_lines.no_element loop
					et_pcb.type_stencil_lines.update_element (
						container	=> module.board.stencil.bottom.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- ARCS TOP
				arcs_cursor := module.board.stencil.top.arcs.first;
				while arcs_cursor /= et_pcb.type_stencil_arcs.no_element loop
					et_pcb.type_stencil_arcs.update_element (
						container	=> module.board.stencil.top.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				-- ARCS BOTTOM
				arcs_cursor := module.board.stencil.bottom.arcs.first;
				while arcs_cursor /= et_pcb.type_stencil_arcs.no_element loop
					et_pcb.type_stencil_arcs.update_element (
						container	=> module.board.stencil.bottom.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;			

				-- CIRCLES TOP
				circles_cursor := module.board.stencil.top.circles.first;
				while circles_cursor /= et_pcb.type_stencil_circles.no_element loop
					et_pcb.type_stencil_circles.update_element (
						container	=> module.board.stencil.top.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- CIRCLES BOTTOM
				circles_cursor := module.board.stencil.bottom.circles.first;
				while circles_cursor /= et_pcb.type_stencil_circles.no_element loop
					et_pcb.type_stencil_circles.update_element (
						container	=> module.board.stencil.bottom.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- POLYGONS TOP
				polygons_cursor := module.board.stencil.top.polygons.first;
				while polygons_cursor /= et_pcb.type_stencil_polygons.no_element loop
					et_pcb.type_stencil_polygons.update_element (
						container	=> module.board.stencil.top.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				-- POLYGONS BOTTOM
				polygons_cursor := module.board.stencil.bottom.polygons.first;
				while polygons_cursor /= et_pcb.type_stencil_polygons.no_element loop
					et_pcb.type_stencil_polygons.update_element (
						container	=> module.board.stencil.bottom.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;	
		
			end move_stencil;


			procedure move_stop_mask is
				use et_pcb.type_stop_lines;
				lines_cursor : et_pcb.type_stop_lines.cursor;

				use et_pcb.type_stop_arcs;
				arcs_cursor : et_pcb.type_stop_arcs.cursor;

				use et_pcb.type_stop_circles;
				circles_cursor : et_pcb.type_stop_circles.cursor;

				use et_pcb.type_stop_polygons;
				polygons_cursor : et_pcb.type_stop_polygons.cursor;

				use et_pcb.type_texts_with_content;
				texts_cursor : et_pcb.type_texts_with_content.cursor;
				
				stop : constant string (1..16) := "board stop mask ";
				
				procedure move_line (line : in out et_pcb.type_stop_line) is
					use et_pcb;
				begin
					log (stop & "line", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (line), log_threshold + log_threshold_add);

					move (line.start_point);
					move (line.end_point);
					
					log (now & to_string (line), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				procedure move_arc (arc : in out et_pcb.type_stop_arc) is
					use et_pcb;
				begin
					log (stop & "arc", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (arc), log_threshold + log_threshold_add);

					move (arc.center);
					move (arc.start_point);
					move (arc.end_point);
					
					log (now & to_string (arc), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				procedure move_circle (circle : in out et_pcb.type_stop_circle) is
					use et_pcb_coordinates;
				begin
					log (stop & "circle", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & " center" & to_string (circle.center), log_threshold + log_threshold_add);

					move (circle.center);
					
					log (now & " center" & to_string (circle.center), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

				procedure move_polygon (polygon : in out et_pcb.type_stop_polygon) is
					use et_pcb_coordinates;
					use et_pcb.type_polygon_points;
					point_cursor : et_pcb.type_polygon_points.cursor := polygon.corners.first;
					new_points : et_pcb.type_polygon_points.set;

					procedure get_point (point : in type_point_2d) is
					-- Reads a corner point, copies it, moves the copy and inserts the moved
					-- copy in a new set "new_points".
						new_point : type_point_2d := point; -- copy given point
					begin
						log (before & to_string (new_point), log_threshold + log_threshold_add);
						move (new_point); -- move copied point
						log (now & to_string (new_point), log_threshold + log_threshold_add);

						-- insert new point in new_points:
						et_pcb.type_polygon_points.insert (
							container	=> new_points,
							new_item	=> new_point);
						
					end get_point;
					
				begin -- move_polygon
					log (stop & "polygon corner points", log_threshold + log_threshold_add);
					log_indentation_up;

					-- loop through polygon corner points and read one after another:
					while point_cursor /= et_pcb.type_polygon_points.no_element loop

						et_pcb.type_polygon_points.query_element (
							position	=> point_cursor,
							process		=> get_point'access);
						
						next (point_cursor);
					end loop;

					-- Now the new set of polygon corner points is available in "new_points".
					-- new_points replaces the old list of points:
					polygon.corners := new_points;
					
					log_indentation_down;
				end move_polygon;

				procedure move_text (text : in out et_pcb.type_text_with_content) is
					use et_pcb_coordinates;
				begin
					log (stop & "text", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (text.position), log_threshold + log_threshold_add);

					move (text.position);
					
					log (now & to_string (text.position), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_text;

			begin -- move_stop_mask
				
				-- LINES TOP
				lines_cursor := module.board.stop_mask.top.lines.first;
				while lines_cursor /= et_pcb.type_stop_lines.no_element loop
					et_pcb.type_stop_lines.update_element (
						container	=> module.board.stop_mask.top.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- LINES BOTTOM
				lines_cursor := module.board.stop_mask.bottom.lines.first;
				while lines_cursor /= et_pcb.type_stop_lines.no_element loop
					et_pcb.type_stop_lines.update_element (
						container	=> module.board.stop_mask.bottom.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- ARCS TOP
				arcs_cursor := module.board.stop_mask.top.arcs.first;
				while arcs_cursor /= et_pcb.type_stop_arcs.no_element loop
					et_pcb.type_stop_arcs.update_element (
						container	=> module.board.stop_mask.top.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				-- ARCS BOTTOM
				arcs_cursor := module.board.stop_mask.bottom.arcs.first;
				while arcs_cursor /= et_pcb.type_stop_arcs.no_element loop
					et_pcb.type_stop_arcs.update_element (
						container	=> module.board.stop_mask.bottom.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;			

				-- CIRCLES TOP
				circles_cursor := module.board.stop_mask.top.circles.first;
				while circles_cursor /= et_pcb.type_stop_circles.no_element loop
					et_pcb.type_stop_circles.update_element (
						container	=> module.board.stop_mask.top.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- CIRCLES BOTTOM
				circles_cursor := module.board.stop_mask.bottom.circles.first;
				while circles_cursor /= et_pcb.type_stop_circles.no_element loop
					et_pcb.type_stop_circles.update_element (
						container	=> module.board.stop_mask.bottom.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- POLYGONS TOP
				polygons_cursor := module.board.stop_mask.top.polygons.first;
				while polygons_cursor /= et_pcb.type_stop_polygons.no_element loop
					et_pcb.type_stop_polygons.update_element (
						container	=> module.board.stop_mask.top.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				-- POLYGONS BOTTOM
				polygons_cursor := module.board.stop_mask.bottom.polygons.first;
				while polygons_cursor /= et_pcb.type_stop_polygons.no_element loop
					et_pcb.type_stop_polygons.update_element (
						container	=> module.board.stop_mask.bottom.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;	

				-- TEXTS TOP
				texts_cursor := module.board.stop_mask.top.texts.first;
				while texts_cursor /= et_pcb.type_texts_with_content.no_element loop
					et_pcb.type_texts_with_content.update_element (
						container	=> module.board.stop_mask.top.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;

				-- TEXTS BOTTOM
				texts_cursor := module.board.stop_mask.bottom.texts.first;
				while texts_cursor /= et_pcb.type_texts_with_content.no_element loop
					et_pcb.type_texts_with_content.update_element (
						container	=> module.board.stop_mask.bottom.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;
				
			end move_stop_mask;


			procedure move_keepout is
				use et_pcb.type_keepout_lines;
				lines_cursor : et_pcb.type_keepout_lines.cursor;

				use et_pcb.type_keepout_arcs;
				arcs_cursor : et_pcb.type_keepout_arcs.cursor;

				use et_pcb.type_keepout_circles;
				circles_cursor : et_pcb.type_keepout_circles.cursor;

				use et_pcb.type_keepout_polygons;
				polygons_cursor : et_pcb.type_keepout_polygons.cursor;

				keepout : constant string (1..14) := "board keepout ";
				
				procedure move_line (line : in out et_pcb.type_keepout_line) is
					use et_pcb;
				begin
					log (keepout & "line", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (line), log_threshold + log_threshold_add);

					move (line.start_point);
					move (line.end_point);
					
					log (now & to_string (line), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				procedure move_arc (arc : in out et_pcb.type_keepout_arc) is
					use et_pcb;
				begin
					log (keepout & "arc", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (arc), log_threshold + log_threshold_add);

					move (arc.center);
					move (arc.start_point);
					move (arc.end_point);
					
					log (now & to_string (arc), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				procedure move_circle (circle : in out et_pcb.type_keepout_circle) is
					use et_pcb_coordinates;
				begin
					log (keepout & "circle", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & " center" & to_string (circle.center), log_threshold + log_threshold_add);

					move (circle.center);
					
					log (now & " center" & to_string (circle.center), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

				procedure move_polygon (polygon : in out et_pcb.type_keepout_polygon) is
					use et_pcb_coordinates;
					use et_pcb.type_polygon_points;
					point_cursor : et_pcb.type_polygon_points.cursor := polygon.corners.first;
					new_points : et_pcb.type_polygon_points.set;

					procedure get_point (point : in type_point_2d) is
					-- Reads a corner point, copies it, moves the copy and inserts the moved
					-- copy in a new set "new_points".
						new_point : type_point_2d := point; -- copy given point
					begin
						log (before & to_string (new_point), log_threshold + log_threshold_add);
						move (new_point); -- move copied point
						log (now & to_string (new_point), log_threshold + log_threshold_add);

						-- insert new point in new_points:
						et_pcb.type_polygon_points.insert (
							container	=> new_points,
							new_item	=> new_point);
						
					end get_point;
					
				begin -- move_polygon
					log (keepout & "polygon corner points", log_threshold + log_threshold_add);
					log_indentation_up;

					-- loop through polygon corner points and read one after another:
					while point_cursor /= et_pcb.type_polygon_points.no_element loop

						et_pcb.type_polygon_points.query_element (
							position	=> point_cursor,
							process		=> get_point'access);
						
						next (point_cursor);
					end loop;

					-- Now the new set of polygon corner points is available in "new_points".
					-- new_points replaces the old list of points:
					polygon.corners := new_points;
					
					log_indentation_down;
				end move_polygon;

			begin -- move_keepout
				
				-- LINES TOP
				lines_cursor := module.board.keepout.top.lines.first;
				while lines_cursor /= et_pcb.type_keepout_lines.no_element loop
					et_pcb.type_keepout_lines.update_element (
						container	=> module.board.keepout.top.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- LINES BOTTOM
				lines_cursor := module.board.keepout.bottom.lines.first;
				while lines_cursor /= et_pcb.type_keepout_lines.no_element loop
					et_pcb.type_keepout_lines.update_element (
						container	=> module.board.keepout.bottom.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- ARCS TOP
				arcs_cursor := module.board.keepout.top.arcs.first;
				while arcs_cursor /= et_pcb.type_keepout_arcs.no_element loop
					et_pcb.type_keepout_arcs.update_element (
						container	=> module.board.keepout.top.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				-- ARCS BOTTOM
				arcs_cursor := module.board.keepout.bottom.arcs.first;
				while arcs_cursor /= et_pcb.type_keepout_arcs.no_element loop
					et_pcb.type_keepout_arcs.update_element (
						container	=> module.board.keepout.bottom.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;			

				-- CIRCLES TOP
				circles_cursor := module.board.keepout.top.circles.first;
				while circles_cursor /= et_pcb.type_keepout_circles.no_element loop
					et_pcb.type_keepout_circles.update_element (
						container	=> module.board.keepout.top.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- CIRCLES BOTTOM
				circles_cursor := module.board.keepout.bottom.circles.first;
				while circles_cursor /= et_pcb.type_keepout_circles.no_element loop
					et_pcb.type_keepout_circles.update_element (
						container	=> module.board.keepout.bottom.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- POLYGONS TOP
				polygons_cursor := module.board.keepout.top.polygons.first;
				while polygons_cursor /= et_pcb.type_keepout_polygons.no_element loop
					et_pcb.type_keepout_polygons.update_element (
						container	=> module.board.keepout.top.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				-- POLYGONS BOTTOM
				polygons_cursor := module.board.keepout.bottom.polygons.first;
				while polygons_cursor /= et_pcb.type_keepout_polygons.no_element loop
					et_pcb.type_keepout_polygons.update_element (
						container	=> module.board.keepout.bottom.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;	
		
			end move_keepout;

			procedure move_contour is
				use et_pcb.type_pcb_contour_lines;
				lines_cursor : et_pcb.type_pcb_contour_lines.cursor;

				use et_pcb.type_pcb_contour_arcs;
				arcs_cursor : et_pcb.type_pcb_contour_arcs.cursor;

				use et_pcb.type_pcb_contour_circles;
				circles_cursor : et_pcb.type_pcb_contour_circles.cursor;

				contour : constant string (1..14) := "board contour ";
				
				procedure move_line (line : in out et_pcb.type_pcb_contour_line) is
					use et_pcb;
				begin
					log (contour & "line", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (line), log_threshold + log_threshold_add);

					move (line.start_point);
					move (line.end_point);
					
					log (now & to_string (line), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				procedure move_arc (arc : in out et_pcb.type_pcb_contour_arc) is
					use et_pcb;
				begin
					log (contour & "arc", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (arc), log_threshold + log_threshold_add);

					move (arc.center);
					move (arc.start_point);
					move (arc.end_point);
					
					log (now & to_string (arc), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				procedure move_circle (circle : in out et_pcb.type_pcb_contour_circle) is
					use et_pcb_coordinates;
				begin
					log (contour & "circle", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & " center" & to_string (circle.center), log_threshold + log_threshold_add);

					move (circle.center);
					
					log (now & " center" & to_string (circle.center), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

			begin -- move_contour
				
				-- LINES
				lines_cursor := module.board.contour.lines.first;
				while lines_cursor /= et_pcb.type_pcb_contour_lines.no_element loop
					et_pcb.type_pcb_contour_lines.update_element (
						container	=> module.board.contour.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- ARCS
				arcs_cursor := module.board.contour.arcs.first;
				while arcs_cursor /= et_pcb.type_pcb_contour_arcs.no_element loop
					et_pcb.type_pcb_contour_arcs.update_element (
						container	=> module.board.contour.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				-- CIRCLES TOP
				circles_cursor := module.board.contour.circles.first;
				while circles_cursor /= et_pcb.type_pcb_contour_circles.no_element loop
					et_pcb.type_pcb_contour_circles.update_element (
						container	=> module.board.contour.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

			end move_contour;

			
			procedure move_copper is
				use et_pcb.type_copper_lines_pcb;
				lines_cursor : et_pcb.type_copper_lines_pcb.cursor;

				use et_pcb.type_copper_arcs_pcb;
				arcs_cursor : et_pcb.type_copper_arcs_pcb.cursor;

				use et_pcb.type_copper_circles_pcb;
				circles_cursor : et_pcb.type_copper_circles_pcb.cursor;

				use et_pcb.type_copper_polygons_floating;
				polygons_cursor : et_pcb.type_copper_polygons_floating.cursor;

				use et_pcb.type_texts_with_content_pcb;
				texts_cursor : et_pcb.type_texts_with_content_pcb.cursor;

				use et_pcb.type_text_placeholders_copper;
				placeholders_cursor : et_pcb.type_text_placeholders_copper.cursor;
				
				board_copper : constant string (1..13) := "board copper ";
				
				procedure move_line (line : in out et_pcb.type_copper_line_pcb) is
					use et_pcb;
				begin
					log (board_copper & "line", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (line), log_threshold + log_threshold_add);

					move (line.start_point);
					move (line.end_point);
					
					log (now & to_string (line), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_line;

				procedure move_arc (arc : in out et_pcb.type_copper_arc_pcb) is
					use et_pcb;
				begin
					log (board_copper & "arc", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (arc), log_threshold + log_threshold_add);

					move (arc.center);
					move (arc.start_point);
					move (arc.end_point);
					
					log (now & to_string (arc), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_arc;

				procedure move_circle (circle : in out et_pcb.type_copper_circle_pcb) is
					use et_pcb_coordinates;
				begin
					log (board_copper & "circle", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & " center" & to_string (circle.center), log_threshold + log_threshold_add);

					move (circle.center);
					
					log (now & " center" & to_string (circle.center), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_circle;

				procedure move_polygon (polygon : in out et_pcb.type_copper_polygon_floating) is
					use et_pcb_coordinates;
					use et_pcb.type_polygon_points;
					point_cursor : et_pcb.type_polygon_points.cursor := polygon.corners.first;
					new_points : et_pcb.type_polygon_points.set;

					procedure get_point (point : in type_point_2d) is
					-- Reads a corner point, copies it, moves the copy and inserts the moved
					-- copy in a new set "new_points".
						new_point : type_point_2d := point; -- copy given point
					begin
						log (before & to_string (new_point), log_threshold + log_threshold_add);
						move (new_point); -- move copied point
						log (now & to_string (new_point), log_threshold + log_threshold_add);

						-- insert new point in new_points:
						et_pcb.type_polygon_points.insert (
							container	=> new_points,
							new_item	=> new_point);
						
					end get_point;
					
				begin -- move_polygon
					log (board_copper & "polygon corner points", log_threshold + log_threshold_add);
					log_indentation_up;

					-- loop through polygon corner points and read one after another:
					while point_cursor /= et_pcb.type_polygon_points.no_element loop

						et_pcb.type_polygon_points.query_element (
							position	=> point_cursor,
							process		=> get_point'access);
						
						next (point_cursor);
					end loop;

					-- Now the new set of polygon corner points is available in "new_points".
					-- new_points replaces the old list of points:
					polygon.corners := new_points;
					
					log_indentation_down;
				end move_polygon;

				procedure move_text (text : in out et_pcb.type_text_with_content_pcb) is
					use et_pcb_coordinates;
				begin
					log (board_copper & "text", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (text.position), log_threshold + log_threshold_add);

					move (text.position);
					
					log (now & to_string (text.position), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_text;

				procedure move_placeholder (text : in out et_pcb.type_text_placeholder_copper) is
					use et_pcb_coordinates;
				begin
					log (board_copper & "text placeholder", log_threshold + log_threshold_add);
					log_indentation_up;

					log (before & to_string (text.position), log_threshold + log_threshold_add);

					move (text.position);
					
					log (now & to_string (text.position), log_threshold + log_threshold_add);
							
					log_indentation_down;
				end move_placeholder;

			begin -- move_copper
				
				-- LINES
				lines_cursor := module.board.copper.lines.first;
				while lines_cursor /= et_pcb.type_copper_lines_pcb.no_element loop
					et_pcb.type_copper_lines_pcb.update_element (
						container	=> module.board.copper.lines,
						position	=> lines_cursor,
						process		=> move_line'access);
					
					next (lines_cursor);
				end loop;

				-- ARCS
				arcs_cursor := module.board.copper.arcs.first;
				while arcs_cursor /= et_pcb.type_copper_arcs_pcb.no_element loop
					et_pcb.type_copper_arcs_pcb.update_element (
						container	=> module.board.copper.arcs,
						position	=> arcs_cursor,
						process		=> move_arc'access);
					
					next (arcs_cursor);
				end loop;

				-- CIRCLES
				circles_cursor := module.board.copper.circles.first;
				while circles_cursor /= et_pcb.type_copper_circles_pcb.no_element loop
					et_pcb.type_copper_circles_pcb.update_element (
						container	=> module.board.copper.circles,
						position	=> circles_cursor,
						process		=> move_circle'access);
					
					next (circles_cursor);
				end loop;

				-- POLYGONS
				polygons_cursor := module.board.copper.polygons.first;
				while polygons_cursor /= et_pcb.type_copper_polygons_floating.no_element loop
					et_pcb.type_copper_polygons_floating.update_element (
						container	=> module.board.copper.polygons,
						position	=> polygons_cursor,
						process		=> move_polygon'access);
					
					next (polygons_cursor);
				end loop;

				-- TEXTS
				texts_cursor := module.board.copper.texts.first;
				while texts_cursor /= et_pcb.type_texts_with_content_pcb.no_element loop
					et_pcb.type_texts_with_content_pcb.update_element (
						container	=> module.board.copper.texts,
						position	=> texts_cursor,
						process		=> move_text'access);

					next (texts_cursor);
				end loop;

				-- TEXT PLACEHOLDERS
				placeholders_cursor := module.board.copper.placeholders.first;
				while placeholders_cursor /= et_pcb.type_text_placeholders_copper.no_element loop
					et_pcb.type_text_placeholders_copper.update_element (
						container	=> module.board.copper.placeholders,
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
			move_copper; -- non-electric copper stuff !!!
		end move_general_board_stuff;

		procedure flatten_netlist (
		-- Changes the path and y position of ports.
		-- NOTE: The netlist contains nets with their connected ports.
			module_name	: in kicad_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is

			use et_kicad.type_netlist;
			net_cursor : et_kicad.type_netlist.cursor := module.netlist.first;

			procedure query_ports (
				net_name	: in type_net_name.bounded_string;
				ports		: in out et_kicad.type_ports_with_reference.set) is

				use et_kicad.type_ports_with_reference;
				port_cursor : et_kicad.type_ports_with_reference.cursor := ports.first;
				port : et_kicad.type_port_with_reference;
				
				use et_coordinates;
			begin -- query_ports
				log ("net " & et_general.to_string (net_name), log_threshold + 3);
				log_indentation_up;

				-- Loop in ports of given net and change path and y position.
				while port_cursor /= et_kicad.type_ports_with_reference.no_element loop
					port := element (port_cursor); -- load the port as it currently is
					
					log (et_libraries.to_string (port.reference)
						& " port "
						& et_libraries.to_string (port.name), log_threshold + 4);
					log_indentation_up;

					-- show old position
					log (before & kicad_coordinates.to_string (
						position => port.coordinates, scope => kicad_coordinates.MODULE),
						log_threshold + 5);

					-- change path
					kicad_coordinates.set_path (port.coordinates, root);

					-- Move position from negative to positive y.
					move (port.coordinates);

					-- show new position
					log (now & kicad_coordinates.to_string (
						position => port.coordinates, scope => kicad_coordinates.MODULE),
						log_threshold + 5);

					-- replace old port by new port
					et_kicad.type_ports_with_reference.replace_element (
						container		=> ports,
						position		=> port_cursor,
						new_item		=> port);
					
					log_indentation_down;
					next (port_cursor);
				end loop;
				
				log_indentation_down;
			end query_ports;
			
		begin -- flatten_netlist
			log ("netlist ...", log_threshold + 2);
			log_indentation_up;

			while net_cursor /= et_kicad.type_netlist.no_element loop
				
				et_kicad.type_netlist.update_element (
					container	=> module.netlist,
					position	=> net_cursor,
					process		=> query_ports'access);
				
				next (net_cursor);
			end loop;
			
			log_indentation_down;
		end flatten_netlist;
		
	begin -- transpose
		log ("transposing coordinates of KiCad modules ...", log_threshold);
		log_indentation_up;
		
		while module_cursor /= et_kicad.type_modules.no_element loop
			log ("module " & kicad_coordinates.to_string (key (module_cursor)), log_threshold + 1);
			log_indentation_up;

			-- log ("schematic ...", log_threshold + 1);
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
				container	=> et_kicad.modules,
				position	=> module_cursor,
				process		=> flatten_notes'access);

			-- CS: The design import (see function read_schematic in kicad.adb) does not read the title block and 
			-- drawing frame lines and texts. Currently only content of things like company name and comments
			-- is read.
			update_element (
				container	=> et_kicad.modules,
				position	=> module_cursor,
				process		=> flatten_frames'access);

			update_element (
				container	=> et_kicad.modules,
				position	=> module_cursor,
				process		=> flatten_components'access);

			update_element (
				container	=> et_kicad.modules,
				position	=> module_cursor,
				process		=> flatten_nets'access);

			update_element (
				container	=> et_kicad.modules,
				position	=> module_cursor,
				process		=> flatten_netlist'access);
			
			-- general non-component related board stuff (silk screen, documentation, ...):
			if board_available then
				update_element (
					container	=> et_kicad.modules,
					position	=> module_cursor,
					process		=> move_general_board_stuff'access);
			end if;
			
			next (module_cursor);
			log_indentation_down;
		end loop;
		
		log_indentation_down;
	end transpose;

	
	function to_native_coordinates (point : in kicad_coordinates.type_coordinates)
	-- Converts kicad schematic coordinates to native schematic coordinates.
		return et_coordinates.type_coordinates is
		point_out : et_coordinates.type_coordinates;
	begin
		point_out := et_coordinates.to_coordinates (
				point => point, -- x,y
				sheet => kicad_coordinates.sheet (point) -- sheet
				);

		return point_out;
	end;
	
	procedure to_native (log_threshold : in et_string_processing.type_log_level) is
	-- Converts the kicad module (incl. component libraries) to a native module.
	-- Converts the packages (from package_libraries) to native packages.
	-- NOTE: Packages of the board (incl. their deviations/modifications
	-- from the package_libraries) are ignored !
	-- Saves the module in project_path (see below) in a module file (*.mod).

		-- When the native project is created we need a project path and a project name:
		project_path : et_project.type_et_project_path.bounded_string :=
						et_project.type_et_project_path.to_bounded_string (
							compose (et_general.work_directory, et_project.directory_import));

		prefix_devices_dir : et_kicad_general.type_device_library_name.bounded_string := -- libraries/devices
			et_libraries.to_file_name (compose (
				et_project.directory_libraries, et_project.directory_libraries_devices));
	
		prefix_packages_dir : et_kicad_general.type_package_library_name.bounded_string := -- libraries/packages
			et_libraries.to_file_name (compose (
				et_project.directory_libraries, et_project.directory_libraries_packages));

		-- Since V4 package libraries are stored in et_kicad_pcb.package_libraries
		-- the copy/convert process must be performed only once.
		-- This flag goes true once V4 package libraries have been converted.
		packages_v4_copied : boolean := false;
		
		use et_kicad.type_modules;
		module_cursor_kicad : et_kicad.type_modules.cursor := et_kicad.type_modules.first (et_kicad.modules);

		-- This is a single native target module used as scratch.
		module : et_schematic.type_module; 

		function to_texts (texts_in : et_kicad.type_texts.list) return et_schematic.type_texts.list is
		-- Converts kicad texts to native texts.
			texts_out : et_schematic.type_texts.list;

			procedure query_texts (cursor : in et_kicad.type_texts.cursor) is
				text_kicad : et_kicad.type_text := et_kicad.type_texts.element (cursor);
				text_native : et_schematic.type_text;
			begin
				-- copy the coordinates x,y,sheet from kicad text to native text
				text_native.coordinates := to_native_coordinates (text_kicad.coordinates);
				
				-- copy the content
				text_native.content := text_kicad.content;

				-- append native text to list of native texts
				et_schematic.type_texts.append (
					container	=> texts_out,
					new_item	=> text_native);

			end query_texts;
			
		begin -- to_texts
			et_kicad.type_texts.iterate (texts_in, query_texts'access);
			return texts_out;
		end;
		
		procedure copy_general_stuff is begin
			module.board_available	:= element (module_cursor_kicad).board_available;
			module.texts			:= to_texts (element (module_cursor_kicad).notes); 
			module.board			:= et_pcb.type_board (element (module_cursor_kicad).board);
			module.net_classes		:= element (module_cursor_kicad).net_classes;
		end copy_general_stuff;

		function concatenate_lib_name_and_generic_name (
			library	: in et_kicad_general.type_device_library_name.bounded_string; -- ../../lbr/bel_logic.lib
			device	: in et_kicad.type_component_generic_name.bounded_string) -- 7400

			-- The return is a composition of prefix_devices_dir, library containing directory,
			-- generic component name and device model extension 
			-- like: libraries/devices/__#__#lbr#bel_logic_7400.dev
			return et_libraries.type_device_model_file.bounded_string is

			use et_libraries;
			use et_kicad_general.type_device_library_name;
			dir : et_kicad_general.type_device_library_name.bounded_string; -- ../../lbr
			name : et_libraries.type_device_model_file.bounded_string; -- to be returned -- libraries/devices/__#__#lbr#bel_logic_7400.dev

			-- In the containing directory . and / must be replaced by _ and #:
			characters : character_mapping := to_mapping ("./","_#");
			
		begin -- concatenate_lib_name_and_generic_name
			dir := to_file_name (containing_directory (et_libraries.to_string (library)) & '#'); -- ../../lbr
			translate (dir, characters); -- __#__#lbr
			--log ("dir " & et_libraries.to_string (dir));
			
			name := to_file_name (base_name (et_libraries.to_string (library))); -- bel_logic
			name := dir & name;
			--log ("name " & et_libraries.to_string (name));

			name := name & '_' & et_libraries.to_file_name (et_kicad.to_string (device));
			--log ("name " & et_libraries.to_string (name));

			name := et_libraries.to_file_name (compose (
					containing_directory	=> et_libraries.to_string (prefix_devices_dir),
					name					=> et_libraries.to_string (name),
					extension				=> et_libraries.device_library_file_extension));

			--log ("name " & et_libraries.to_string (name));
			
			return name;
		end concatenate_lib_name_and_generic_name;

		function rename_package_model (
			model_in : in et_kicad_general.type_package_library_name.bounded_string) -- ../../lbr/transistors.pretty/S_0805
			return et_libraries.type_package_model_file.bounded_string is
			-- The return is something like: libraries/packages/__#__#lbr#transistors.pretty_S_0805.pac .

			use et_libraries;
			use et_kicad_general.type_package_library_name;

			-- In the containing directory . and / must be replaced by _ and #:
			characters : character_mapping := to_mapping ("./","_#");

			model_copy : et_kicad_general.type_package_library_name.bounded_string := model_in; -- ../../lbr/transistors.pretty/S_0805
			model_return : et_libraries.type_package_model_file.bounded_string;
		begin -- rename_package_model
			translate (model_copy, characters);

			model_return := et_libraries.to_file_name (compose (
					containing_directory	=> et_libraries.to_string (prefix_packages_dir),
					name					=> et_libraries.to_string (model_copy),
					extension				=> et_pcb.library_file_extension));

			return model_return;
		end rename_package_model;
		
		procedure copy_components is
		-- Transfer components from kicad module to native module.
		-- Changes the links to device models so that they point to the libraries
		-- in project/libraries/devices/...
			use et_schematic;
			use et_kicad.type_components_schematic;
			components_kicad		: et_kicad.type_components_schematic.map;
			component_cursor_kicad	: et_kicad.type_components_schematic.cursor;

			use et_schematic.type_devices;
			component_cursor_native	: et_schematic.type_devices.cursor;
			component_inserted		: boolean;

			procedure copy_units (
			-- Copies the kicad units to the native component.
				reference	: in et_libraries.type_device_name;
				component	: in out et_schematic.type_device) is

				use et_kicad.type_units_schematic;
				units_kicad			: et_kicad.type_units_schematic.map := element (component_cursor_kicad).units;
				unit_cursor_kicad	: et_kicad.type_units_schematic.cursor := units_kicad.first; -- point to first unit

				use et_schematic.type_units;
				unit_cursor_native	: et_schematic.type_units.cursor;
				unit_inserted		: boolean;

				unit_native_virtual	: et_schematic.type_unit (et_libraries.SCH);
				unit_native_real	: et_schematic.type_unit (et_libraries.SCH_PCB);
			begin -- copy_units
				log_indentation_up;
				
				while unit_cursor_kicad /= et_kicad.type_units_schematic.no_element loop
					log ("unit " & et_libraries.to_string (key (unit_cursor_kicad)), log_threshold + 3);

					-- depending on the appearance of the kicad component, we create a virtual or real 
					-- unit in the native schematic module:

					-- The units can be obtained by converting the kicad unit to the base unit (see et_schematic.type_unit_base)
					-- and adding stuff of real components (if real device).
					-- Kicad stuff like "alternative representation", package name, datasheet is discarded.
					case element (component_cursor_kicad).appearance is
						when et_libraries.SCH => -- virtual device

							unit_native_virtual := (et_schematic.type_unit_base (element (unit_cursor_kicad)) with 
											position	=> to_native_coordinates (element (unit_cursor_kicad).position),
											appearance	=> et_libraries.SCH);

							et_schematic.type_units.insert (
								container	=> component.units,
								key			=> key (unit_cursor_kicad),
								position	=> unit_cursor_native,
								inserted	=> unit_inserted,
								new_item	=> unit_native_virtual);

						when et_libraries.SCH_PCB => -- real device

							unit_native_real := (et_schematic.type_unit_base (element (unit_cursor_kicad)) with 
											position	=> to_native_coordinates (element (unit_cursor_kicad).position),
											appearance	=> et_libraries.SCH_PCB,
											
											-- and stuff that comes with a real device:
											
												reference	=> element (unit_cursor_kicad).reference,
												value		=> element (unit_cursor_kicad).value,
												
												-- create a placeholder for purpose because kicad does not know such a thing
												purpose		=> (meaning => et_libraries.PURPOSE, others => <>)
												);
							
							et_schematic.type_units.insert (
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
			-- load a copy of kicad schematic components
			components_kicad := element (module_cursor_kicad).components;
			
			-- loop in the component list of the kicad schematic module
			component_cursor_kicad := components_kicad.first;
			while component_cursor_kicad /= et_kicad.type_components_schematic.no_element loop

				log ("component " & et_libraries.to_string (key (component_cursor_kicad)), log_threshold + 2);
				
				-- depending on the appearance of the kicad component, we create a virtual or real 
				-- component in the native schematic module.
				-- Kicad stuff like the boolean power_flag is ignored.
				case element (component_cursor_kicad).appearance is
					when et_libraries.SCH =>
						
						et_schematic.type_devices.insert (
							container	=> module.devices,
							key			=> key (component_cursor_kicad), -- PWR04, FLG01
							position	=> component_cursor_native,
							new_item	=> (
								appearance			=> et_libraries.SCH,

								-- The link to the device model is a composition of path,file and generic name:
								model				=> concatenate_lib_name_and_generic_name (
														library	=> element (component_cursor_kicad).library_name,
														device	=> element (component_cursor_kicad).generic_name),

								-- NOTE: The value of virtual components (like power symbols) is discarded here.
								
								others 				=> <>), -- unit list is empty at this time

							inserted	=> component_inserted); -- should always be true

					when et_libraries.SCH_PCB => null;
						et_schematic.type_devices.insert (
							container	=> module.devices,
							key			=> key (component_cursor_kicad), -- IC308, R12
							position	=> component_cursor_native,
							new_item	=> (
								appearance			=> et_libraries.SCH_PCB,

								-- The link to the device model is a composition of path,file and generic name:
								model				=> concatenate_lib_name_and_generic_name (
														library	=> element (component_cursor_kicad).library_name,
														device	=> element (component_cursor_kicad).generic_name),

								value				=> element (component_cursor_kicad).value,
								partcode			=> et_libraries.to_partcode (et_libraries.partcode_default), -- not provided by kicad
								purpose				=> et_schematic.to_purpose (et_schematic.purpose_default), -- not provided by kicad
								bom					=> YES, -- in kicad there is no bom status -> assume part is mounted
								variant				=> element (component_cursor_kicad).variant,

								position			=> element (component_cursor_kicad).position,
								text_placeholders	=> element (component_cursor_kicad).text_placeholders,
								others 				=> <>), -- unit list is empty at this time

							inserted	=> component_inserted); -- should always be true
				end case;

				-- copy the units from the kicad component to the native device
				et_schematic.type_devices.update_element (
					container	=> module.devices,
					position	=> component_cursor_native,
					process		=> copy_units'access);

				next (component_cursor_kicad);

			end loop;
		end copy_components;

		procedure copy_nets is
			use et_schematic;
			
			use et_kicad.type_nets;
			use et_kicad.type_strands;
			kicad_nets			: et_kicad.type_nets.map := element (module_cursor_kicad).nets;
			kicad_net_cursor	: et_kicad.type_nets.cursor := kicad_nets.first;

			use et_schematic.type_nets;
			net_cursor_native	: et_schematic.type_nets.cursor;
			net_inserted		: boolean;

			procedure insert_strands (
			-- copies the kicad strands to native strands of a net.
			-- Strand names (from kicad) are discarded. ET does not provide a name for a strand.
			-- As a strand is part of a net, there is no need for individual strand names.
				net_name	: in type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is

				use et_kicad.type_strands;
				kicad_strands : et_kicad.type_strands.list := element (kicad_net_cursor).strands;
				kicad_strand_cursor : et_kicad.type_strands.cursor := kicad_strands.first;
								
				use et_kicad.type_net_segments;
				kicad_segments : et_kicad.type_net_segments.list;
				kicad_segment_cursor : et_kicad.type_net_segments.cursor;

				use et_schematic.type_strands;
				strands_native : et_schematic.type_strands.list;
				strand_native : et_schematic.type_strand;
			
				use et_schematic.type_net_segments;
				net_segments_native : et_schematic.type_net_segments.list;
				net_segment_native : et_schematic.type_net_segment;

				use et_schematic.type_net_labels;
				use et_schematic.type_junctions;
				use et_schematic.type_ports_device;
				
				function tag_and_simple_labels (segment : in et_kicad.type_net_segment) 
				-- Copies from the given kicad net segment all simple and tag labels and returns
				-- them in a single list.
					return et_schematic.type_net_labels.list is
					labels : et_schematic.type_net_labels.list; -- to be returned

					use et_kicad.type_simple_labels;
					simple_label_cursor : et_kicad.type_simple_labels.cursor := segment.label_list_simple.first;

					use et_kicad.type_tag_labels;
					tag_label_cursor : et_kicad.type_tag_labels.cursor := segment.label_list_tag.first;

				begin -- tag_and_simple_labels
					log_indentation_up;
					
					-- simple labels
					while simple_label_cursor /= et_kicad.type_simple_labels.no_element loop

						log ("simple label" & et_kicad.to_string (
							label => et_kicad.type_net_label (element (simple_label_cursor))),
							log_threshold + 5);
						
						et_schematic.type_net_labels.append (
							container	=> labels,
							new_item	=> (
								appearance	=> et_schematic.SIMPLE,
								coordinates	=> element (simple_label_cursor).coordinates,
								orientation	=> element (simple_label_cursor).orientation,
								size		=> element (simple_label_cursor).size,
								style		=> element (simple_label_cursor).style,
								width		=> element (simple_label_cursor).width)
						);
						
						next (simple_label_cursor);
					end loop;

					-- tag labels
					while tag_label_cursor /= et_kicad.type_tag_labels.no_element loop

						log ("tag label" & et_kicad.to_string (
							label => et_kicad.type_net_label (element (tag_label_cursor))),
							log_threshold + 5);
						
						et_schematic.type_net_labels.append (
							container	=> labels,
							new_item	=> (
								appearance	=> et_schematic.TAG,
								coordinates	=> element (tag_label_cursor).coordinates,
								orientation	=> element (tag_label_cursor).orientation,
								size		=> element (tag_label_cursor).size,
								style		=> element (tag_label_cursor).style,
								width		=> element (tag_label_cursor).width,
								direction	=> element (tag_label_cursor).direction
								)
						);
						
						next (tag_label_cursor);
					end loop;

					log_indentation_down;
					return labels;
				end tag_and_simple_labels;

				function read_net_junctions (segment : in et_kicad.type_net_segment)
				-- Collects net junctions given in segment and returns them in a single list.
					return et_schematic.type_junctions.list is
					junctions : et_schematic.type_junctions.list; -- to be returned

					use et_kicad.type_junctions;
					junction_cursor : et_kicad.type_junctions.cursor := segment.junctions.first;
					junction_native : et_schematic.type_net_junction;
				begin
					log_indentation_up;
					while junction_cursor /= et_kicad.type_junctions.no_element loop

						log ("junction" & et_coordinates.to_string (
							point => element (junction_cursor).coordinates),
							log_threshold + 5);
						
						-- copy the x/y position of kicad junction to native junction
						et_coordinates.set_xy (
							point		=> junction_native.coordinates,
							position	=> et_coordinates.type_point (element (junction_cursor).coordinates));
						
						et_schematic.type_junctions.append (
							container	=> junctions,
							new_item	=> junction_native);
						
						next (junction_cursor);
					end loop;
					log_indentation_down;
					return junctions;
				end read_net_junctions;

				function read_net_junctions_2 (segment : in et_kicad.type_net_segment)
				-- Iterates junctions of segment. Tests if they sit on start or end point
				-- and sets the corresponding flag in the native junctions of the native segment.
				-- Issues warning on misplaced junction. The misplaced junction is discarded.
				-- By this mechanism excessive junctions (sitting on top of each other) 
				-- are reduced to a single junction (without warning).
					return et_schematic.type_junctions_2 is
					junctions : et_schematic.type_junctions_2; -- to be returned

					use kicad_coordinates;
					use et_kicad.type_junctions;
					junction_cursor : et_kicad.type_junctions.cursor := segment.junctions.first;
				begin
					log_indentation_up;
					
					while junction_cursor /= et_kicad.type_junctions.no_element loop

						log ("junction" & et_coordinates.to_string (
							point => element (junction_cursor).coordinates),
							log_threshold + 5);

						-- Test if junction sits at start point of segment:
						if element (junction_cursor).coordinates = segment.coordinates_start then
							junctions.start_point := true;

						-- Test if junction sits at end point of segment:
						elsif element (junction_cursor).coordinates = segment.coordinates_end then
							junctions.end_point := true;

						-- If junction misplaced, issue warning:
						else
							log (message_warning & 
								"misplaced junction between start and end point of segment -> ignored !");
						end if;
						
						next (junction_cursor);
					end loop;
					
					log_indentation_down;
					return junctions;
				end read_net_junctions_2;
				
				function read_ports (segment : in et_kicad.type_net_segment)
				-- Returns the component ports connected with the given net segment.
					return et_schematic.type_ports_device.set is

					use et_kicad.type_ports_with_reference;
					port_cursor_kicad : et_kicad.type_ports_with_reference.cursor;
					all_ports_of_net : et_kicad.type_ports_with_reference.set;
					
					ports_of_segment : et_schematic.type_ports_device.set; -- to be returned

					use et_coordinates;
					use et_geometry;
					distance : et_geometry.type_distance_point_from_line;
				begin -- read_ports
					log_indentation_up;
					
					-- get all ports connected with the current net (in the kicad module):
					all_ports_of_net := et_kicad.components_in_net (
						module			=> key (module_cursor_kicad), -- the name of the kicad module
						net				=> net_name, -- the net in question 
						log_threshold	=> log_threshold + 6);

					-- Loop in all ports of the net. 
					-- Select the ports which are on the same sheet as the current strand.
					-- Select the ports which are connected with the given net segment.
					port_cursor_kicad := all_ports_of_net.first;
					while port_cursor_kicad /= et_kicad.type_ports_with_reference.no_element loop

						-- compare sheet numbers
						if 	kicad_coordinates.sheet (element (port_cursor_kicad).coordinates) = 
							kicad_coordinates.sheet (element (kicad_strand_cursor).position) then

							-- calculate distance of port from segment
							distance := distance_of_point_from_line (
								point 		=> type_point (element (port_cursor_kicad).coordinates),
								line_start	=> type_point (segment.coordinates_start),
								line_end	=> type_point (segment.coordinates_end),
								line_range	=> with_end_points);

							-- If port sits on segment, append it to ports_of_segment.
							if (not distance.out_of_range) and distance.distance = zero_distance then
								log (et_libraries.to_string (element (port_cursor_kicad).reference) 
									 & " port "
									 & et_libraries.to_string (element (port_cursor_kicad).name)
									 & kicad_coordinates.to_string (
											position	=> element (port_cursor_kicad).coordinates,
											scope		=> kicad_coordinates.XY),
									 log_threshold + 5);

								et_schematic.type_ports_device.insert (
									container	=> ports_of_segment,
									new_item	=> (
											device_name	=> element (port_cursor_kicad).reference,
											port_name	=> element (port_cursor_kicad).name));
							end if;

						end if;
						next (port_cursor_kicad);
					end loop;
					
					log_indentation_down;
					return ports_of_segment;
				end read_ports;

-- 				function to_junctions (junctions : in et_kicad.type_junctions.list) 
-- 					return et_schematic.type_junctions.list is
-- 				-- Copies the kicad junctions to a list of native junctions.
-- 					junctions_out : et_schematic.type_junctions.list;
-- 
-- 					procedure query_junction (cursor : et_kicad.type_junctions.cursor) is
-- 						junction_kicad	: et_kicad.type_net_junction := et_kicad.type_junctions.element (cursor);
-- 						junction_native	: et_schematic.type_net_junction;
-- 					begin
-- 						-- A native junction contains x and y only.
-- 						junction_native.coordinates := et_coordinates.type_point (junction_kicad.coordinates);
-- 
-- 						-- Append native junction to native list of junctions:
-- 						et_schematic.type_junctions.append (junctions_out, junction_native);
-- 					end query_junction;
-- 					
-- 				begin -- to_junctions
-- 					et_kicad.type_junctions.iterate (junctions, query_junction'access);
-- 					return junctions_out;
-- 				end to_junctions;
				
			begin -- insert_strands
				log_indentation_up;
				
				-- loop in strands of current kicad net
				while kicad_strand_cursor /= et_kicad.type_strands.no_element loop
					log ("strand" & kicad_coordinates.to_string (
						 position	=> element (kicad_strand_cursor).position,
						 scope		=> kicad_coordinates.SHEET),
						 log_threshold + 3);
					
					-- load segments of current strand
					kicad_segments := element (kicad_strand_cursor).segments;
					kicad_segment_cursor := kicad_segments.first;

					-- loop in segments of current strand
					-- A kicad net segment has labels and junctions.
					log_indentation_up;
					while kicad_segment_cursor /= et_kicad.type_net_segments.no_element loop

						log ("segment" & et_kicad.to_string (
							segment		=> element (kicad_segment_cursor),
							scope		=> kicad_coordinates.XY),
							log_threshold + 4);
						
						-- get coordinates from current kicad net segment:
						net_segment_native.coordinates_start := et_coordinates.type_point (element (kicad_segment_cursor).coordinates_start);
						net_segment_native.coordinates_end   := et_coordinates.type_point (element (kicad_segment_cursor).coordinates_end);

						-- get labels from current kicad net segment
						net_segment_native.labels := tag_and_simple_labels (element (kicad_segment_cursor));

						-- read net junctions of the current segment
						--net_segment_native.junctions := read_net_junctions (element (kicad_segment_cursor));
						net_segment_native.junctions_2 := read_net_junctions_2 (element (kicad_segment_cursor));

						-- read ports connected with the segment
						net_segment_native.ports_devices := read_ports (element (kicad_segment_cursor));

						-- there are no ports of submodules
						net_segment_native.ports_submodules := type_ports_submodule.empty_set;
						
						-- Collect native net segment in list net_segments_native.
						et_schematic.type_net_segments.append (
							container	=> net_segments_native,
							new_item	=> net_segment_native);

						next (kicad_segment_cursor);
					end loop;
					log_indentation_down;

					-- get lowest x/y coordinates of current kicad strand:
					strand_native.position := to_native_coordinates (element (kicad_strand_cursor).position);

					-- copy net segments to native strand
					strand_native.segments := net_segments_native;

					-- clear collection of net segments (for the next strand)
					clear (net_segments_native);
					
					-- collect native strand (incl. segments) in list strands_native
					et_schematic.type_strands.append (
						container	=> strands_native,
						new_item	=> strand_native);
					
					next (kicad_strand_cursor);
				end loop;

				net.strands := strands_native;

				log_indentation_down;
			end insert_strands;

			procedure copy_layout_stuff (
				net_name	: in type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
			begin -- copy_layout_stuff
				log_indentation_up;

				log ("class" & et_pcb.to_string (element (kicad_net_cursor).class), log_threshold + 3);
				net.class := element (kicad_net_cursor).class;	
				
				log ("tracks, vias, polygons ...", log_threshold + 3);
				net.route := element (kicad_net_cursor).route;
				-- CS log details on tracks, vias, ...
				
				log_indentation_down;
			end copy_layout_stuff;
											
		begin -- copy_nets
			-- loop in kicad nets
			while kicad_net_cursor /= et_kicad.type_nets.no_element loop
				log ("net " & et_general.to_string (key (kicad_net_cursor)), log_threshold + 2);

				et_schematic.type_nets.insert (
					container	=> module.nets,
					position	=> net_cursor_native,
					inserted	=> net_inserted,
					key			=> key (kicad_net_cursor), -- net name
					new_item	=> (
							-- convert the kicad net scope to native net scope
							scope	=> et_schematic.type_net_scope'value (et_kicad.to_string (element (kicad_net_cursor).scope)),
							others 	=> <>)
					);

				-- insert strands (schematic related)
				et_schematic.type_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor_native,
					process		=> insert_strands'access);

				-- copy layout related stuff (copper segments, vias, ...)
				et_schematic.type_nets.update_element (
					container	=> module.nets,
					position	=> net_cursor_native,
					process		=> copy_layout_stuff'access);
				
				next (kicad_net_cursor);
			end loop;
		end copy_nets;

		procedure copy_frames is
		-- Converts the kicad drawing frame templates (schematic and layout) to native templates.
		-- CS: not completed yet.
		-- For the time being the native module gets dummy templates assigned.
		begin
			module.frame_template_schematic := et_libraries.frame_template_name_dummy;
			module.frame_template_board := et_libraries.frame_template_name_dummy;
		end copy_frames;
		
		procedure copy_libraries (
			module_name : in kicad_coordinates.type_submodule_name.bounded_string;
			module		: in et_kicad.type_module) is
			
			-- This cursor points to the kicad component library being converted:
			use et_kicad.type_libraries;
			component_library_cursor : et_kicad.type_libraries.cursor := module.component_libraries.first;

			use et_kicad_general.type_device_library_name;
			component_library_name : et_kicad_general.type_device_library_name.bounded_string; -- lbr/logic.lib

			-- This cursor points to the kicad footprint library being converted:			
			use et_kicad_pcb.type_libraries;
			package_library_cursor : et_kicad_pcb.type_libraries.cursor := module.footprints.first;

			use et_libraries.type_package_model_file;
			
			procedure query_components (
				library_name	: in et_kicad_general.type_device_library_name.bounded_string; -- lbr/logig.lib
				library			: in et_kicad.type_components_library.map) is

				use et_kicad.type_components_library;
				component_cursor : et_kicad.type_components_library.cursor := library.first;

				use et_kicad.type_component_generic_name;
				generic_name : et_kicad.type_component_generic_name.bounded_string; -- 7400
				device_model : et_libraries.type_device_model_file.bounded_string; -- ../lbr/logic_ttl/7400.dev

				device_cursor : et_libraries.type_devices.cursor;
				inserted : boolean;

				procedure copy_units (
				-- Transfers the kicad units to native units in the current native ET device.
					device_name	: in et_libraries.type_device_model_file.bounded_string; -- libraries/devices/transistors/pnp.dev
					device		: in out et_libraries.type_device) is

					-- Make a copy of the kicad units of the current kicad component:
					units_kicad : et_kicad.type_units_library.map := element (component_cursor).units;

					-- This cursor points to a kicad unit:
					use et_kicad.type_units_library;
					unit_cursor_kicad : et_kicad.type_units_library.cursor := units_kicad.first;

					-- Here we store temporarily the ports of a kicad unit:
					ports_kicad : et_kicad.type_ports_library.list;
					
					-- This cursor points to a native ET unit.
					unit_cursor : et_libraries.type_units_internal.cursor;
					inserted	: boolean;

					procedure copy_ports (
						unit_name	: in et_libraries.type_unit_name.bounded_string;
						unit		: in out et_libraries.type_unit_internal) is

						function to_level (style : in et_kicad.type_port_style) 
						-- Maps from kicad port style to native port characteristic.
							return et_libraries.type_sensitivity_level is 
							use et_kicad;
							use et_libraries;
						begin
							case style is
								when INVERTED | INVISIBLE_INVERTED | INVISIBLE_INPUT_LOW |
									INPUT_LOW | OUTPUT_LOW | INVISIBLE_OUTPUT_LOW => return LOW;
								
								when others => return HIGH;
							end case;
						end to_level;

						function to_edge (style : in et_kicad.type_port_style) 
						-- Maps from kicad port style to native port characteristic.
							return et_libraries.type_sensitivity_edge is 
							use et_kicad;
							use et_libraries;
						begin
							case style is
								when CLOCK | INVISIBLE_CLOCK | RISING_EDGE_CLK |
									INVISIBLE_RISING_EDGE_CLK => return RISING;
								
								when INVERTED_CLOCK | INVISIBLE_INVERTED_CLOCK | INVISIBLE_CLOCK_LOW |
									FALLING_EDGE_CLK | INVISIBLE_FALLING_EDGE_CLK => return FALLING;
								
								when others => return NONE;
							end case;
						end to_edge;

						function to_inverted (style : in et_kicad.type_port_style) 
						-- Maps from kicad port style to native port characteristic.
							return et_libraries.type_output_inverted is 
							use et_kicad;
							use et_libraries;
						begin
							case style is
								when INVERTED | OUTPUT_LOW | INVISIBLE_INVERTED | 
									INVISIBLE_OUTPUT_LOW => return YES;
								
								when others => return NO;
							end case;
						end to_inverted;
						
						-- This cursor points to a port of a kicad unit. We initialize it so that
						-- it points to the first port of the current unit.
						use et_kicad.type_ports_library;
						port_cursor_kicad : et_kicad.type_ports_library.cursor := ports_kicad.first;

						use et_libraries;
						port_inserted : boolean;
						port_cursor : et_libraries.type_ports.cursor;
						
					begin -- copy_ports
						-- Loop in kicad ports and append them to the current native unit portlist.
						-- Depending on the direction and style, the kicad port gets translated to a native port.
						-- If the kicad component comes with multiple ports of the same name, only the first
						-- port is copied. A symbol is an abstraction of a function block. There is no need for 
						-- multiple ports having the same name.
						while port_cursor_kicad /= et_kicad.type_ports_library.no_element loop

							case element (port_cursor_kicad).direction is
								when et_kicad.PASSIVE | et_kicad.UNKNOWN =>
									et_libraries.type_ports.insert (
										container	=> unit.symbol.ports,
										key			=> element (port_cursor_kicad).name,
										position	=> port_cursor,
										inserted	=> port_inserted,
										new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
											direction			=> et_libraries.PASSIVE));

								when et_kicad.INPUT =>
									case element (port_cursor_kicad).style is
										when et_kicad.NON_LOGIC | et_kicad.INVISIBLE_NON_LOGIC =>
											et_libraries.type_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
													direction			=> INPUT_ANALOG));

										when others => -- all other styles indicate a digital input
											et_libraries.type_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,																			   
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
													direction			=> INPUT_DIGITAL,
													sensitivity_edge	=> to_edge (element (port_cursor_kicad).style),
													sensitivity_level	=> to_level (element (port_cursor_kicad).style)));
									end case;

								when et_kicad.OUTPUT =>
									case element (port_cursor_kicad).style is
										when et_kicad.NON_LOGIC | et_kicad.INVISIBLE_NON_LOGIC =>
											et_libraries.type_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_ANALOG,
													output_analog_tristate	=> NO,
													output_analog_weakness	=> NONE));

										when others => -- all other styles indicate a digital output
											et_libraries.type_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_DIGITAL,
													output_digital_inverted => to_inverted (element (port_cursor_kicad).style),
													output_digital_tristate => NO,											
													output_digital_weakness => NONE));
									end case;
											
								when et_kicad.TRISTATE =>
									case element (port_cursor_kicad).style is
										when et_kicad.NON_LOGIC | et_kicad.INVISIBLE_NON_LOGIC =>
											et_libraries.type_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_ANALOG,
													output_analog_tristate	=> YES,
													output_analog_weakness	=> NONE));
											
										when others => -- all other styles indicate a digital output
											et_libraries.type_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_DIGITAL,
													output_digital_inverted => to_inverted (element (port_cursor_kicad).style),
													output_digital_tristate => YES,
													output_digital_weakness => NONE));
									end case;
									
								when et_kicad.WEAK0 =>
									case element (port_cursor_kicad).style is
										when et_kicad.NON_LOGIC | et_kicad.INVISIBLE_NON_LOGIC =>
											et_libraries.type_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_ANALOG,
													output_analog_tristate	=> NO,
													output_analog_weakness	=> WEAK0));
											
										when others => -- all other styles indicate a digital output
											et_libraries.type_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_DIGITAL,
													output_digital_inverted	=> to_inverted (element (port_cursor_kicad).style),
													output_digital_tristate	=> NO,
													output_digital_weakness	=> WEAK0));
									end case;
									
								when et_kicad.WEAK1 =>
									case element (port_cursor_kicad).style is
										when et_kicad.NON_LOGIC | et_kicad.INVISIBLE_NON_LOGIC =>
											et_libraries.type_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_ANALOG,
													output_analog_tristate	=> NO,
													output_analog_weakness	=> WEAK1));
											
										when others => -- all other styles indicate a digital output
											et_libraries.type_ports.insert (
												container	=> unit.symbol.ports,
												key			=> element (port_cursor_kicad).name,
												position	=> port_cursor,
												inserted	=> port_inserted,
												new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
													direction				=> OUTPUT_DIGITAL,
													output_digital_inverted	=> to_inverted (element (port_cursor_kicad).style),
													output_digital_tristate	=> NO,
													output_digital_weakness	=> WEAK1));
									end case;
									
								when et_kicad.BIDIR =>
									et_libraries.type_ports.insert (
										container	=> unit.symbol.ports,
										key			=> element (port_cursor_kicad).name,
										position	=> port_cursor,
										inserted	=> port_inserted,
										new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
											direction			=> BIDIR_DIGITAL,
											output_inverted		=> to_inverted (element (port_cursor_kicad).style),
											output_tristate		=> NO,
											output_weakness		=> NONE,
											input_sensitivity_edge	=> to_edge (element (port_cursor_kicad).style),
											input_sensitivity_level	=> to_level (element (port_cursor_kicad).style)));

								when et_kicad.POWER_OUT =>
									et_libraries.type_ports.insert (
										container	=> unit.symbol.ports,
										key			=> element (port_cursor_kicad).name,
										position	=> port_cursor,
										inserted	=> port_inserted,
										new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
											direction			=> POWER_OUT,
											level				=> LEVEL_ZERO)); 
											-- CS: The level could be reasoned from the port name such as +12V or -5V.

								when et_kicad.POWER_IN =>
									et_libraries.type_ports.insert (
										container	=> unit.symbol.ports,
										key			=> element (port_cursor_kicad).name,
										position	=> port_cursor,
										inserted	=> port_inserted,
										new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
											direction			=> POWER_IN,
											level				=> LEVEL_ZERO)); 
											-- CS: The level could be reasoned from the port name such as +12V or -5V.

								when et_kicad.NOT_CONNECTED =>
									et_libraries.type_ports.insert (
										container	=> unit.symbol.ports,
										key			=> element (port_cursor_kicad).name,
										position	=> port_cursor,
										inserted	=> port_inserted,
										new_item	=> (et_libraries.type_port_base (element (port_cursor_kicad)) with
											direction			=> NOT_CONNECTED));

									-- NOTE: The kicad port_name_offset is discarded here.

							end case;
							
							next (port_cursor_kicad);
						end loop;
					end copy_ports;

					function convert_shapes (shapes : in et_kicad.type_symbol_shapes) 
						return et_libraries.type_shapes is

						native_shapes : et_libraries.type_shapes;

						procedure copy_line (cursor : in et_kicad.type_symbol_lines.cursor) is begin
							et_libraries.type_lines.append (
								container	=> native_shapes.lines,
								new_item	=> et_kicad.type_symbol_lines.element (cursor));
						end;

						procedure copy_arc (cursor : in et_kicad.type_symbol_arcs.cursor) is begin
							et_libraries.type_arcs.append (
								container	=> native_shapes.arcs,
								new_item	=> et_libraries.type_arc (et_kicad.type_symbol_arcs.element (cursor)));
						end;

						procedure copy_circle (cursor : in et_kicad.type_symbol_circles.cursor) is begin
							et_libraries.type_circles.append (
								container	=> native_shapes.circles,
								new_item	=> (
									et_libraries.type_circle_base (et_kicad.type_symbol_circles.element (cursor))
									with filled => et_libraries.NO));
						end;						

						procedure copy_polyline (cursor : in et_kicad.type_symbol_polylines.cursor) is 
						-- Converts a polyline to single lines and appends them to native_shapes.lines.
							use et_kicad;
							use type_symbol_points;

							-- This is the given kicad polyline:
							polyline : type_symbol_polyline := type_symbol_polylines.element (cursor);

							-- This cursor points to a particular point of the polyline:
							point_cursor : type_symbol_points.cursor := polyline.points.first;

							-- This is the native line that will be appended to native.shapes.lines:
							line : et_libraries.type_line := (width => polyline.width, others => <>);

							-- This flag indicates whether a start or an end point of a line is expected:
							start : boolean := true; -- when start point -> true, when end point -> false
						begin
							-- Advance through points of polyline and assign line start and and points.
							-- Then append the line to native.shapes.lines.
							while point_cursor /= type_symbol_points.no_element loop

								case start is
									when TRUE =>
										-- The point is a start point if another point follows. Otherwise nothing happens.
										if next (point_cursor) /= type_symbol_points.no_element then
											line.start_point := element (point_cursor); -- start point
											start := false; -- up next: end point
										end if;

									when FALSE =>
										line.end_point := element (point_cursor); -- end point
										start := true; -- up next: start point

										-- append line to collection of native lines
										et_libraries.type_lines.append (
											container	=> native_shapes.lines,
											new_item	=> line);

										-- Set cursor one point back so that this point serves as start point
										-- for the next segment.
										previous (point_cursor);

								end case;

								next (point_cursor); -- advance to next point
							end loop;
						end copy_polyline;

						procedure copy_rectangle (cursor : in et_kicad.type_symbol_rectangles.cursor) is
						-- Converts a rectangle to four lines and appends them to native_shapes.lines.
							use et_kicad;
							use type_symbol_rectangles;
							use et_coordinates;

							-- This is the given kicad rectangle:
							rectangle : type_symbol_rectangle := type_symbol_rectangles.element (cursor);

							-- This is the native line that will be appended to native_shapes.lines:
							line : et_libraries.type_line := (width => rectangle.width, others => <>);
							width, height : et_coordinates.type_distance;
							corner_C, corner_D : type_point;
							
							procedure append_line is begin
								et_libraries.type_lines.append (
									container	=> native_shapes.lines,
									new_item	=> line);
							end;
								
						begin -- copy_rectangle
							-- compute width and height of the rectangle:
							width  := distance (axis => X, point_2 => rectangle.corner_B, point_1 => rectangle.corner_A);
							height := distance (axis => Y, point_2 => rectangle.corner_B, point_1 => rectangle.corner_A);

							-- compute corner points of the rectangle:
							-- corner_A is the lower left corner of the rectangle -> already known by the given rectangle
							-- corner_B is the upper right corner of the rectangle -> already known by the given rectangle

							-- corner_C is the lower right corner:
							corner_C := type_point (set_point (
								x => distance (X, rectangle.corner_A) + width,
								y => distance (Y, rectangle.corner_A)
								));

							-- corner_D is the upper left corner:
							corner_D := type_point (set_point (
								x => distance (X, rectangle.corner_A),
								y => distance (Y, rectangle.corner_A) + height
								));
							
							-- lower horizontal line
							line.start_point := rectangle.corner_A;
							line.end_point := corner_C;
							append_line;
							
							-- upper horizontal line
							line.start_point := corner_D;
							line.end_point := rectangle.corner_B;
							append_line;

							-- left vertical line
							line.start_point := rectangle.corner_A;
							line.end_point := corner_D;
							append_line;

							-- right vertical line
							line.start_point := corner_C;
							line.end_point := rectangle.corner_B;
							append_line;

						end copy_rectangle;

					begin -- convert_shapes
						et_kicad.type_symbol_lines.iterate (shapes.lines, copy_line'access);
						et_kicad.type_symbol_arcs.iterate (shapes.arcs, copy_arc'access);
						et_kicad.type_symbol_circles.iterate (shapes.circles, copy_circle'access);

						et_kicad.type_symbol_polylines.iterate (shapes.polylines, copy_polyline'access);
						et_kicad.type_symbol_rectangles.iterate (shapes.rectangles, copy_rectangle'access);
						return native_shapes;
					end convert_shapes;
					
				begin -- copy_units
					while unit_cursor_kicad /= et_kicad.type_units_library.no_element loop

						-- Copy the portlist of the current unit. It is required when ports are inserted in the native unit.
						ports_kicad := element (unit_cursor_kicad).symbol.ports;

						-- create internal units
						case element (unit_cursor_kicad).appearance is
							when et_libraries.SCH_PCB => -- real
						
								et_libraries.type_units_internal.insert (
									container	=> device.units_internal,
									key			=> key (unit_cursor_kicad), -- the name of the unit
									position	=> unit_cursor, -- set unit_cursor for later updating the current unit
									inserted	=> inserted,
									new_item	=> (
										appearance	=> et_libraries.SCH_PCB, -- real !
										position	=> element (unit_cursor_kicad).coordinates,
										swap_level	=> <>,
										add_level	=> <>, -- CS depends on the "global" flag. When true add_level should be "request"

										-- If the unit is real, then the symbol is real too:
										symbol		=> (et_libraries.type_symbol_base (element (unit_cursor_kicad).symbol)
														with 
															shapes		=> convert_shapes (element (unit_cursor_kicad).symbol.shapes),
															appearance	=> et_libraries.SCH_PCB,
															ports		=> et_libraries.type_ports.empty_map, -- ports will come later
															reference	=> element (unit_cursor_kicad).symbol.reference, -- placeholder
															value		=> element (unit_cursor_kicad).symbol.value, -- placeholder
															purpose		=> ( -- we must invent a placeholder for purpose since kicad does not know such a thing
																	meaning	=> et_libraries.PURPOSE,
																	others 	=> <>))
															-- NOTE: Other placeholders (fields in kicad) discarded here.
										));

							when et_libraries.SCH => -- virtual
								et_libraries.type_units_internal.insert (
									container	=> device.units_internal,
									key			=> key (unit_cursor_kicad), -- the name of the unit
									position	=> unit_cursor, -- set unit_cursor for later updating the current unit
									inserted	=> inserted,
									new_item	=> (
										appearance	=> et_libraries.SCH, -- virtual !
										position	=> element (unit_cursor_kicad).coordinates,
										swap_level	=> <>,
										add_level	=> <>, -- CS depends on the "global" flag. When true add_level should be "request"

										-- If the unit is virtual, then the symbol is virtual too:
										symbol		=> (et_libraries.type_symbol_base (element (unit_cursor_kicad).symbol)
														with 
															shapes		=> convert_shapes (element (unit_cursor_kicad).symbol.shapes),
															appearance	=> et_libraries.SCH,
															ports		=> et_libraries.type_ports.empty_map) -- ports will come later
															-- NOTE: Other placeholders discarded here.
										));

							when others => raise constraint_error; -- CS
						end case;
								
						-- copy ports 
						et_libraries.type_units_internal.update_element (
							container	=> device.units_internal,
							position	=> unit_cursor,
							process		=> copy_ports'access);
						
						next (unit_cursor_kicad);
					end loop;
				end copy_units;

				procedure rename_package_model_in_variants (
				-- The package associated with a variant must be changed so that it becomes 
				-- something like libraries/packages/__#__#lbr#transistors.pretty_S_0805.pac
					device_name	: in et_libraries.type_device_model_file.bounded_string; -- libraries/devices/transistors/pnp.dev
					device		: in out et_libraries.type_device) is

					use et_libraries.type_component_variants;
					variant_cursor : et_libraries.type_component_variants.cursor := device.variants.first;

					procedure rename (
						variant_name	: in et_libraries.type_component_variant_name.bounded_string; -- N, D, ...
						variant			: in out et_libraries.type_component_variant) is
					begin -- rename
						variant.package_model := (rename_package_model (variant.package_model)); -- ../../lbr/transistors.pretty/S_0805

						log ("package variant " & et_libraries.to_string (variant_name) 
							 & " now uses package " 
							 & et_libraries.to_string (variant.package_model), log_threshold + 4);
					end rename;
					
				begin -- rename_package_model_in_variants
					-- Loop in variants and rename the package names.
					while variant_cursor /= et_libraries.type_component_variants.no_element loop

						et_libraries.type_component_variants.update_element (
							container	=> device.variants,
							position	=> variant_cursor,
							process		=> rename'access);
						
						next (variant_cursor);
					end loop;
				end rename_package_model_in_variants;

				function remove_leading_hash (
				-- Removes the leading hash character from the prefix of a virtual component like #FLG or #PWR.
					prefix : in et_libraries.type_device_name_prefix.bounded_string) return
					et_libraries.type_device_name_prefix.bounded_string is
					use et_libraries.type_device_name_prefix;
				begin
					return et_libraries.to_prefix (slice (prefix, 2, length (prefix))); -- FLG, PWR
				end;
				
			begin -- query_components
				while component_cursor /= et_kicad.type_components_library.no_element loop
					generic_name := et_kicad.strip_tilde (key (component_cursor));
					--log ("device " & to_string (generic_name), log_threshold + 2);

					-- Build the name of the device model from the component library name and generic name:
					device_model := concatenate_lib_name_and_generic_name (component_library_name, generic_name); -- ../lbr/logic_ttl/7400.dev

					-- Create a new device model in container et_libraries.devices:
					log ("device model " & to_string (device_model), log_threshold + 3);
					log_indentation_up;

					case element (component_cursor).appearance is
						when et_libraries.SCH =>
							et_libraries.type_devices.insert (
								container	=> et_libraries.devices,
								position	=> device_cursor,
								inserted	=> inserted,
								key			=> device_model,
								new_item	=> (
									appearance		=> et_libraries.SCH,
									prefix 			=> remove_leading_hash (element (component_cursor).prefix),
									units_internal	=> <>, -- internal units will come later
									units_external	=> <> -- kicad components do not have external symbols
									-- NOTE: Kicad value of power symbols is discarded.
									-- NOTE: KiCad power_flag is discarded.
								));

						when et_libraries.SCH_PCB =>
							et_libraries.type_devices.insert (
								container	=> et_libraries.devices,
								position	=> device_cursor,
								inserted	=> inserted,
								key			=> device_model,
								new_item	=> (
									appearance		=> et_libraries.SCH_PCB,
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
									et_libraries.type_devices.update_element (
										container	=> et_libraries.devices,
										position	=> device_cursor,
										process		=> rename_package_model_in_variants'access);
								end if;
									
						when others =>
							raise constraint_error;
					end case;

					-- If multiple designs are converted, a particular device might be 
					-- in et_libraries.devices already. The flag "inserted" would be false 
					-- in this case. Copying units must not take place again:
					if inserted then
						-- Copy units.
						et_libraries.type_devices.update_element (
							container	=> et_libraries.devices,
							position	=> device_cursor,
							process		=> copy_units'access);
					else
						log ("already there -> skipped", log_threshold + 3);
					end if;
					
					log_indentation_down;
					
					next (component_cursor);
				end loop;
			end query_components;


			procedure query_packages (
			-- Creates with the library name and package name new native package models.
				library_name	: in et_kicad_general.type_package_library_name.bounded_string; -- projects/lbr/smd_packages.pretty
				library			: in et_kicad_pcb.type_packages_library.map) is

				use et_kicad_pcb.type_packages_library;
				package_cursor_kicad	: et_kicad_pcb.type_packages_library.cursor := library.first;
				package_name			: et_libraries.type_component_package_name.bounded_string;
				package_model			: et_libraries.type_package_model_file.bounded_string := library_name; -- projects/lbr/smd_packages.pretty

				use et_pcb.type_packages;
				package_cursor			: et_pcb.type_packages.cursor;
				inserted				: boolean;
			begin -- query_packages
				-- Loop in kicad packages (footprints) of the current library.
				while package_cursor_kicad /= et_kicad_pcb.type_packages_library.no_element loop
					package_name := key (package_cursor_kicad); -- S_0805
					--log ("package name " & et_libraries.to_string (package_name), log_threshold + 2);

					-- build the new native package model name
					package_model := et_libraries.to_file_name (compose (
										containing_directory	=> et_libraries.to_string (library_name), -- projects/lbr/smd_packages.pretty
										name					=> et_libraries.to_string (package_name))); -- S_0805

					-- replace . and / in package_model 
					package_model := rename_package_model (package_model);
					log ("package model " & et_libraries.to_string (package_model), log_threshold + 3);

					-- Insert the new package model in et_pcb.packages. In case the package is already in the 
					-- container (due to other project imports), the flag "inserted" will go false. The package
					-- would not be inserted again:
					et_pcb.type_packages.insert (
						container	=> et_pcb.packages,
						key			=> package_model, -- libraries/packages/#home#user#lbr#bel_battery_pretty#S_CR3232.pac
						position	=> package_cursor,
						inserted	=> inserted,
						new_item	=> (et_pcb.type_package_base (element (package_cursor_kicad))
										with 
										silk_screen				=> element (package_cursor_kicad).silk_screen,
										assembly_documentation	=> element (package_cursor_kicad).assembly_documentation,
										terminals 				=> element (package_cursor_kicad).terminals)
										-- NOTE: The kicad timestamp is discarded here.
						);
					
					next (package_cursor_kicad);
				end loop;
			end query_packages;

										 
		begin -- copy_libraries

			-- Loop in kicad component libraries:
			while component_library_cursor /= et_kicad.type_libraries.no_element loop
				component_library_name := key (component_library_cursor);
				log ("component library " & to_string (component_library_name), log_threshold + 2);

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
						package_library_cursor := et_kicad_pcb.package_libraries.first;

						-- Loop in footprint libraries:
						while package_library_cursor /= et_kicad_pcb.type_libraries.no_element loop
							log ("package library " & to_string (key (package_library_cursor)), log_threshold + 2);

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
					while package_library_cursor /= et_kicad_pcb.type_libraries.no_element loop
						log ("package library " & to_string (key (package_library_cursor)), log_threshold + 2);

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


		procedure save_libraries (
		-- Saves the library containers (et_libraries.devices and et_pcb.packages) in
		-- the directory specified by project_path and project_name.
			project_name	: in et_project.type_project_name.bounded_string;		-- blood_sample_analyzer
			project_path	: in et_project.type_et_project_path.bounded_string; 	-- /home/user/et_projects/imported_from_kicad
			log_threshold	: in et_string_processing.type_log_level) is
			use et_project;
			use type_project_name;
			use type_et_project_path;
			use ada.directories;
			use et_string_processing;

			package type_path is new generic_bounded_length (project_name_max + project_path_max + 1); -- incl. directory separator
			use type_path;
			path : type_path.bounded_string := to_bounded_string (
					compose (type_et_project_path.to_string (project_path), type_project_name.to_string (project_name)));
			-- Path now contains something like /home/user/et_projects/imported_from_kicad/blood_sample_analyzer
			
			use et_libraries.type_devices;

			procedure save_device (device_cursor : in et_libraries.type_devices.cursor) is
				use et_libraries;
			begin
				et_project.save_device (
					-- library name like: 
					-- /home/user/et_projects/imported_from_kicad/blood_sample_analyzer/libraries/devices/__#__#lbr#bel_connector_and_jumper_FEMALE_01X06.dev
					name	=> to_string (path) & gnat.directory_operations.dir_separator & to_string (key (device_cursor)),

					-- the device model itself:
					device	=> element (device_cursor),
					log_threshold	=> log_threshold + 1); 
			end save_device;

			use et_pcb.type_packages;
			
			procedure save_package (package_cursor : in et_pcb.type_packages.cursor) is
				use et_libraries.type_package_model_file;
			begin
				et_project.save_package (
					-- package name like: 
					-- /home/user/et_projects/imported_from_kicad/blood_sample_analyzer/libraries/packages/__#__#lbr#bel_connector_and_jumper_FEMALE_01X06.pac
					name	=> to_string (path) & gnat.directory_operations.dir_separator & to_string (key (package_cursor)),

					-- the package model itself:
					packge	=> element (package_cursor),
					log_threshold	=> log_threshold + 1); 
			end save_package;
			
		begin -- save_libraries
			log ("saving libraries ...", log_threshold);
			log_indentation_up;

			log ("devices (former KiCad components) ...", log_threshold + 1);
			log_indentation_up;
			iterate (et_libraries.devices, save_device'access);
			log_indentation_down;
			
			log ("packages (former KiCad footprints) ...", log_threshold + 1);
			log_indentation_up;
			iterate (et_pcb.packages, save_package'access);
			log_indentation_down;

			log_indentation_down;			
		end save_libraries;

		use et_project.type_project_name;
		project_name : et_project.type_project_name.bounded_string; -- blood_sample_analyzer

	begin -- to_native
	
		-- First, the kicad designs (currently there is only one) must be flattened so that we get real flat designs.
		-- Further-on the y coordinates of objects in schematics and layouts must be changed. 
		-- Kicad schematic has origin in upper left corner. ET has origin in lower left corder.
		transpose (log_threshold);

		log ("converting ...", log_threshold);
		log_indentation_up;

		-- Now we copy content from the kicad module to the same named native module.
		-- CS: currently there is only one kicad and only one native module.
		while module_cursor_kicad /= et_kicad.type_modules.no_element loop

			-- Copy the kicad module name to the native project name.
			-- The native project name and the module contained will have the same name.
			project_name := et_project.to_project_name (kicad_coordinates.to_string (key (module_cursor_kicad)));
			
			log ("module " & to_string (project_name), log_threshold + 1);
			log_indentation_up;

			-- For each kicad design we create a native project:
			et_project.create_project_directory (
				project_name	=> project_name, 		-- blood_sample_analyzer
				project_path	=> project_path, 		-- /home/user/et_projects/imported_from_kicad
				log_threshold 	=> log_threshold + 2);
			
			-- Clear scratch module because in the following everything goes there.
			module := (others => <>);
			
			copy_general_stuff;

			copy_components;

			copy_nets;
			
			copy_frames; -- CS: not completed yet

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
			
			-- save module in file *.mod
			et_project.save_module (
				module			=> module, -- the module it is about
				project_name	=> project_name, -- blood_sample_analyzer
				project_path	=> project_path, -- /home/user/et_projects/imported_from_kicad
				log_threshold	=> log_threshold);

			-- save libraries (from et_libraries.devices and et_pcb.packages 
			-- to native project directory libraries/devices and libraries/packages)
			save_libraries (
				project_name	=> project_name, -- blood_sample_analyzer
				project_path	=> project_path, -- /home/user/et_projects/imported_from_kicad
				log_threshold 	=> log_threshold + 1);

			
			log_indentation_down;
			next (module_cursor_kicad);
		end loop;


		
		log_indentation_down;

		exception
			when event: others =>

				-- output the line of code where the exception occured:
				et_string_processing.show_line (file => gnat.source_info.file, line => gnat.source_info.line);
				raise;
		
	end to_native;
	

	
end et_kicad_to_native;

-- Soli Deo Gloria
