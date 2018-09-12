------------------------------------------------------------------------------
--                                                                          --
--                       SYSTEM ET KICAD_TO_NATIVE                          --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2018 Mario Blunk, Blunk electronic                 --
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
with ada.exceptions; 			use ada.exceptions;

with et_coordinates;
with et_libraries;
with et_schematic;
with et_configuration;
with et_geometry;

with et_general;
with et_string_processing;		use et_string_processing;
with et_project;
with et_pcb;
with et_pcb_coordinates;
with et_kicad;
with et_kicad_pcb;
with et_export;
with et_csv;

package body et_kicad_to_native is

	procedure flatten (log_threshold : in et_string_processing.type_log_level) is
	-- Flattens schematic of the kicad project: 
	-- Changes the path (selector of et_coordinates.type_coordinates) to the root path (/).
	-- This procedure also moves schematic objects from negative to positive y coordinates.
	-- The origin in kicad is the upper left corner. The origin in ET is the lower left corner.
		use et_kicad.type_rig;
		module_cursor : et_kicad.type_rig.cursor := et_kicad.type_rig.first (et_kicad.rig);

		root : et_coordinates.type_path_to_submodule.list := et_coordinates.type_path_to_submodule.empty_list;
-- 		before	: constant string (1..15) := "position before";
-- 		now		: constant string (1..15) := "position now   ";
		before	: constant string (1..6) := "before";
		now		: constant string (1..6) := "now   ";

		-- This list of frames serves to map from sheet number to paper size:
		frames : et_libraries.type_frames.list;

		function paper_size_of_schematic_sheet (sheet_number : in et_coordinates.type_submodule_sheet_number)
		-- Returns for a given sheet number the respective paper size.
			return et_general.type_paper_size is

			-- This is to be returned. In case no paper size was found, use the default value of type_paper_size.
			size : et_general.type_paper_size := et_general.type_paper_size'first;

			sheet_found : boolean := false; -- goes true once the given sheet has been found
		
			procedure query_sheet_number (frame : in et_libraries.type_frame) is
				use et_coordinates;
			begin
				if et_coordinates.sheet (frame.coordinates) = sheet_number then
					size := frame.paper_size;
					sheet_found := true;
				end if;
			end query_sheet_number;

			-- We search for the paper size in the list "frames":
			use et_libraries.type_frames;
			frame_cursor : et_libraries.type_frames.cursor := frames.first;
			
		begin -- query_frames

			-- loop in list of frames given in "frames"
			while frame_cursor /= et_libraries.type_frames.no_element loop
				
				query_element (
					position	=> frame_cursor,
					process		=> query_sheet_number'access);

				if sheet_found then exit; end if; -- cancel search once the given sheet has been found
				
				next (frame_cursor);
			end loop;

			if not sheet_found then
				log_indentation_reset;
				log (message_error & "sheet with number" & et_coordinates.to_string (sheet_number) & " not found !");
				raise constraint_error;
			end if;
			
			return size;
		end paper_size_of_schematic_sheet;

		procedure move (point : in out et_coordinates.type_coordinates) is
		-- Transposes a point from the kicad frame to the ET native frame.
		-- KiCad frames have the origin in the upper left corner.
		-- ET frames have the origin in the lower left corner.
			use et_coordinates;
			sheet_number 		: et_coordinates.type_submodule_sheet_number;
			sheet_paper_size	: et_general.type_paper_size;
			sheet_height		: et_coordinates.type_distance_xy;
			new_y				: et_coordinates.type_distance_xy;
		begin
			-- get the sheet number where the given point resides
			sheet_number		:= sheet (point); 

			-- get the paper size of the sheet
			sheet_paper_size	:= paper_size_of_schematic_sheet (sheet_number);

			-- get the paper height of the sheet
			sheet_height		:= paper_dimension (axis => Y, paper_size => sheet_paper_size);

			-- calculate the new y position
			new_y				:= sheet_height - distance_y (point);

			-- assign the new y position to the given point
			set_y (point, new_y);
		end move;

		procedure move (
			point_actual	: in out et_coordinates.type_2d_point;	-- the point it is about
			point_help		: in et_coordinates.type_coordinates	-- supportive point that proviedes the sheet number
			) is
		-- Transposes the point_actual from the kicad frame to the ET native frame.
		-- point_help has supporting purpose: it provides the sheet number where point_actual sits.
		-- KiCad frames have the origin in the upper left corner.
		-- ET frames have the origin in the lower left corner.
			use et_coordinates;
			sheet_number 		: et_coordinates.type_submodule_sheet_number;
			sheet_paper_size	: et_general.type_paper_size;
			sheet_height		: et_coordinates.type_distance_xy;
			new_y				: et_coordinates.type_distance_xy;
		begin
			-- get the sheet number where the given point resides
			sheet_number		:= sheet (point_help); 

			-- get the paper size of the sheet
			sheet_paper_size	:= paper_size_of_schematic_sheet (sheet_number);

			-- get the paper height of the sheet
			sheet_height		:= paper_dimension (axis => Y, paper_size => sheet_paper_size);

			-- calculate the new y position
			new_y				:= sheet_height - distance_y (point_actual);

			-- assign the new y position to the given point
			set_y (point_actual, new_y);
		end move;
		
		procedure flatten_notes (
			module_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is

			use et_schematic.type_texts;
			note_cursor : et_schematic.type_texts.cursor := module.notes.first;

			procedure change_path (note : in out et_schematic.type_note) is
				use et_coordinates;
			begin
				log ("note '" & et_libraries.to_string (note.content) & "'", log_threshold + 3);
				log_indentation_up;
				
				log (before & to_string (position => note.coordinates, scope => et_coordinates.MODULE),
					 log_threshold + 4);

				et_coordinates.set_path (note.coordinates, root);

				-- Move position from negative to positive y.
				move (note.coordinates);

				log (now & to_string (position => note.coordinates, scope => et_coordinates.MODULE),
					 log_threshold + 4);

				log_indentation_down;
			end change_path;
				
		begin -- flatten_notes
			log ("text notes ...", log_threshold + 2);
			log_indentation_up;
			
			while note_cursor /= et_schematic.type_texts.no_element loop
				et_schematic.type_texts.update_element (
					container	=> module.notes,
					position	=> note_cursor,
					process		=> change_path'access);

				next (note_cursor);
			end loop;

			log_indentation_down;
		end flatten_notes;

		procedure flatten_frames (
			module_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is

			use et_libraries.type_frames;
			frame_cursor : et_libraries.type_frames.cursor := module.frames.first;

			procedure change_path (frame : in out et_libraries.type_frame) is
				use et_coordinates;
			begin
				-- CS what should be logged here ?
				log_indentation_up;
				
				log (before & to_string (position => frame.coordinates, scope => et_coordinates.MODULE),
					 log_threshold + 4);

				et_coordinates.set_path (frame.coordinates, root);

				log (now & to_string (position => frame.coordinates, scope => et_coordinates.MODULE),
					 log_threshold + 4);

				log_indentation_down;
			end change_path;
				
		begin -- flatten_frames
			log ("frames ...", log_threshold + 2);
			log_indentation_up;
			
			while frame_cursor /= et_libraries.type_frames.no_element loop
				et_libraries.type_frames.update_element (
					container	=> module.frames,
					position	=> frame_cursor,
					process		=> change_path'access);

				next (frame_cursor);
			end loop;

			log_indentation_down;
		end flatten_frames;

		procedure flatten_components (
			module_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is

			use et_kicad.type_components_schematic;
			component_cursor : et_kicad.type_components_schematic.cursor := module.components.first;

			procedure query_units (
				reference	: in et_libraries.type_component_reference;
				component	: in out et_kicad.type_component_schematic) is
				-- 				use et_coordinates;
				use et_kicad.type_units_schematic;
				unit_cursor : et_kicad.type_units_schematic.cursor := component.units.first;

				procedure change_path (
					unit_name	: in et_libraries.type_unit_name.bounded_string;
					unit		: in out et_kicad.type_unit_schematic) is
					use et_coordinates;
				begin
					log ("unit " & et_libraries.to_string (unit_name), log_threshold + 4);
					log_indentation_up;
					
					log (before & to_string (position => unit.position, scope => et_coordinates.MODULE),
						log_threshold + 4);

					et_coordinates.set_path (unit.position, root);

					move (unit.position); -- Move position from negative to positive y.

					log (now & to_string (position => unit.position, scope => et_coordinates.MODULE),
						log_threshold + 4);

					log_indentation_down;
				end change_path;

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
			module_name	: in et_coordinates.type_submodule_name.bounded_string;
			module		: in out et_kicad.type_module) is

			use et_kicad.type_nets;
			net_cursor : et_kicad.type_nets.cursor := module.nets.first;

			procedure query_strands (
				net_name	: in et_schematic.type_net_name.bounded_string;
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
							
							-- as supportive point that provides the sheet number we pass the segment start position.
							move (point_actual => label.coordinates, point_help => segment.coordinates_start);

							log ("simple label " & now & to_string (label.coordinates), log_threshold + 3);							
						end move_simple_label;
						
						use et_kicad.type_tag_labels;
						tag_label_cursor : et_kicad.type_tag_labels.cursor := segment.label_list_tag.first;

						procedure move_tag_label (label : in out et_kicad.type_net_label_tag) is
						-- Moves the given tag label from kicad frame to native frame.
						begin
							log ("tag label " & before & to_string (label.coordinates), log_threshold + 3);
							
							-- as supportive point that provides the sheet number we pass the segment start position.
							move (point_actual => label.coordinates, point_help => segment.coordinates_start);
							
							log ("tag label " & now & to_string (label.coordinates), log_threshold + 3);
						end move_tag_label;

					begin -- change_path_of_segment
						log ("segment", log_threshold + 3);
						log_indentation_up;

						-- start point of net segment
						log ("start " & before & to_string (position => segment.coordinates_start, scope => et_coordinates.MODULE),
							log_threshold + 3);

						et_coordinates.set_path (segment.coordinates_start, root);

						move (segment.coordinates_start); -- Move position from negative to positive y.
						
						log ("start " & now & to_string (position => segment.coordinates_start, scope => et_coordinates.MODULE),
							log_threshold + 3);

						-- end point of net segment
						log ("end   " & before & to_string (position => segment.coordinates_end, scope => et_coordinates.MODULE),
							log_threshold + 3);

						et_coordinates.set_path (segment.coordinates_end, root);

						move (segment.coordinates_end); -- Move position from negative to positive y.
						
						log ("end   " & now & to_string (position => segment.coordinates_end, scope => et_coordinates.MODULE),
							log_threshold + 3);

						-- move y of net labels
						while simple_label_cursor /= et_kicad.type_simple_labels.no_element loop
							et_kicad.type_simple_labels.update_element (
								container	=> segment.label_list_simple,
								position	=> simple_label_cursor,
								process 	=> move_simple_label'access);
							next (simple_label_cursor);
						end loop;

						while tag_label_cursor /= et_kicad.type_tag_labels.no_element loop
							et_kicad.type_tag_labels.update_element (
								container	=> segment.label_list_tag,
								position	=> tag_label_cursor,
								process 	=> move_tag_label'access);
							next (tag_label_cursor);
						end loop;

						-- CS change y of junctionsts ?
						
						log_indentation_down;

					end change_path_of_segment;
					
				begin -- query_segments
					while segment_cursor /= et_kicad.type_net_segments.no_element loop

						et_kicad.type_net_segments.update_element (
							container	=> strand.segments,
							position	=> segment_cursor,
							process		=> change_path_of_segment'access);
						
						next (segment_cursor);
					end loop;
				end query_segments;
				
			begin -- query_strands
				while strand_cursor /= et_kicad.type_strands.no_element loop

					et_kicad.type_strands.update_element (
						container	=> net.strands,
						position	=> strand_cursor,
						process		=> query_segments'access);
					
					next (strand_cursor);
				end loop;
			end query_strands;
			
		begin -- flatten_nets
			log ("nets ...", log_threshold + 2);
			log_indentation_up;
			
			while net_cursor /= et_kicad.type_nets.no_element loop
				log (et_schematic.to_string (key (net_cursor)), log_threshold + 3);

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
		
	begin -- flatten
		log ("flattening project ...", log_threshold);
		log_indentation_up;
		
		while module_cursor /= et_kicad.type_rig.no_element loop
			log ("module " & et_coordinates.to_string (key (module_cursor)), log_threshold + 1);
			log_indentation_up;

			-- As preparation for later mirroring the y positions of objects,
			-- we need a list of drawing frames. This list will later provide the
			-- paper sizes of individual drawing sheets.
			frames := element (module_cursor).frames;
			
			update_element (
				container	=> et_kicad.rig,
				position	=> module_cursor,
				process		=> flatten_notes'access);

			-- CS: The design import (see function read_schematic in kicad.adb) does not read the title block and 
			-- drawing frame lines and texts. Currently only content of things like company name and comments
			-- is read.
			update_element (
				container	=> et_kicad.rig,
				position	=> module_cursor,
				process		=> flatten_frames'access);

			update_element (
				container	=> et_kicad.rig,
				position	=> module_cursor,
				process		=> flatten_components'access);

			update_element (
				container	=> et_kicad.rig,
				position	=> module_cursor,
				process		=> flatten_nets'access);
			
			next (module_cursor);

			log_indentation_down;
		end loop;
		
		log_indentation_down;
	end flatten;
	
	procedure to_native (log_threshold : in et_string_processing.type_log_level) is
	-- Converts the rig (inc. component libraries) to a native project.
	-- Converts the packages (from package_libraries) to native packages.
	-- NOTE: Packages of the board (incl. their deviations from the package_libraries) are ignored !

		use et_kicad.type_rig;
		module_cursor_kicad : et_kicad.type_rig.cursor := et_kicad.type_rig.first (et_kicad.rig);

		use et_schematic.type_rig;
		module_cursor_native : et_schematic.type_rig.cursor;
		module_inserted : boolean;

		procedure copy_general_stuff (
			module_name : in et_coordinates.type_submodule_name.bounded_string;
			module		: in out et_schematic.type_module) is
		begin
			module.generic_name	:= element (module_cursor_kicad).generic_name;
			module.instance		:= element (module_cursor_kicad).instance;

			module.notes		:= element (module_cursor_kicad).notes; 
			-- CS: mirror y coordinates

			module.board		:= element (module_cursor_kicad).board;
			-- CS: mirror y coordinates
			
			module.net_classes	:= element (module_cursor_kicad).net_classes;
		end copy_general_stuff;

		procedure copy_components (
			module_name : in et_coordinates.type_submodule_name.bounded_string;
			module		: in out et_schematic.type_module) is

			use et_kicad.type_components_schematic;
			components_kicad		: et_kicad.type_components_schematic.map;
			component_cursor_kicad	: et_kicad.type_components_schematic.cursor;

			use et_schematic.type_components;
			component_cursor_native	: et_schematic.type_components.cursor;
			component_inserted		: boolean;

			procedure copy_units (
			-- Copies the kicad units to the native component.
				reference	: in et_libraries.type_component_reference;
				component	: in out et_schematic.type_component) is

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

					-- The units can be obtained by converting the kicad unit to the base unit (see et_schematic.type_units)
					-- because Kicad units are derived from this base type.
					-- Kicad stuff like path_to_package or alternative representation is discarded.
					case element (component_cursor_kicad).appearance is
						when et_libraries.SCH =>

							unit_native_virtual := et_schematic.type_unit (element (unit_cursor_kicad));
							-- CS: mirror y coordinates
							
							et_schematic.type_units.insert (
								container	=> component.units,
								key			=> key (unit_cursor_kicad),
								position	=> unit_cursor_native,
								inserted	=> unit_inserted,
								new_item	=> unit_native_virtual);

						when et_libraries.SCH_PCB =>

							unit_native_real := et_schematic.type_unit (element (unit_cursor_kicad));
							-- CS: mirror y coordinates
							
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
				-- Kicad stuff like power_flag is discarded.
				case element (component_cursor_kicad).appearance is
					when et_libraries.SCH =>
						
						et_schematic.type_components.insert (
							container	=> module.components,
							key			=> key (component_cursor_kicad), -- IC308, R12
							position	=> component_cursor_native,
							new_item	=> (
								appearance			=> et_libraries.SCH,
								library_name		=> element (component_cursor_kicad).library_name,
								generic_name		=> element (component_cursor_kicad).generic_name,
								value				=> element (component_cursor_kicad).value,
								commissioned		=> element (component_cursor_kicad).commissioned,
								updated				=> element (component_cursor_kicad).updated,
								author				=> element (component_cursor_kicad).author,
								others 				=> <>), -- unit list is empty at this time

							inserted	=> component_inserted); -- should always be true

					when et_libraries.SCH_PCB => null;
						et_schematic.type_components.insert (
							container	=> module.components,
							key			=> key (component_cursor_kicad), -- IC308, R12
							position	=> component_cursor_native,
							new_item	=> (
								appearance			=> et_libraries.SCH_PCB,
								library_name		=> element (component_cursor_kicad).library_name,
								generic_name		=> element (component_cursor_kicad).generic_name,
								value				=> element (component_cursor_kicad).value,
								commissioned		=> element (component_cursor_kicad).commissioned,
								updated				=> element (component_cursor_kicad).updated,
								author				=> element (component_cursor_kicad).author,

								partcode			=> element (component_cursor_kicad).partcode,
								purpose				=> element (component_cursor_kicad).purpose,
								bom					=> element (component_cursor_kicad).bom,
								variant				=> element (component_cursor_kicad).variant,

								position			=> element (component_cursor_kicad).position,
								text_placeholders	=> element (component_cursor_kicad).text_placeholders,
								others 				=> <>), -- unit list is empty at this time

							inserted	=> component_inserted); -- should always be true
				end case;

				-- copy the units from the kicad component to the native component
				et_schematic.type_components.update_element (
					container	=> module.components,
					position	=> component_cursor_native,
					process		=> copy_units'access);

				next (component_cursor_kicad);

			end loop;
		end copy_components;

		procedure copy_nets (
			module_name : in et_coordinates.type_submodule_name.bounded_string;
			module		: in out et_schematic.type_module) is

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
				net_name	: in et_schematic.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is

				use et_kicad.type_strands;
				kicad_strands : et_kicad.type_strands.list := element (kicad_net_cursor).strands;
				kicad_strand_cursor : et_kicad.type_strands.cursor := kicad_strands.first;
								
				use et_kicad.type_net_segments;
				kicad_segments : et_kicad.type_net_segments.list;
				kicad_segment_cursor : et_kicad.type_net_segments.cursor;

				use et_schematic.type_strands;
				strands_native : et_schematic.type_strands.list;
				strand_base : et_schematic.type_strand_base;
			
				use et_schematic.type_net_segments;
				net_segments_native : et_schematic.type_net_segments.list;
				net_segment_base : et_schematic.type_net_segment_base;

				use et_schematic.type_net_labels;
				net_labels_native : et_schematic.type_net_labels.list;

				use et_schematic.type_junctions;
				net_junctions_native : et_schematic.type_junctions.list;

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
						et_coordinates.set_xy (junction_native.coordinates, element (junction_cursor).coordinates);
						
						et_schematic.type_junctions.append (
							container	=> junctions,
							new_item	=> junction_native);
						
						next (junction_cursor);
					end loop;
					log_indentation_down;
					return junctions;
				end read_net_junctions;
				
			begin -- insert_strands
				log_indentation_up;
				
				-- loop in strands of current kicad net
				while kicad_strand_cursor /= et_kicad.type_strands.no_element loop
					log ("strand" & et_coordinates.to_string (
						 position	=> element (kicad_strand_cursor).coordinates,
						 scope		=> et_coordinates.SHEET),
						 log_threshold + 3);
					
					-- load segments of current strand
					kicad_segments := element (kicad_strand_cursor).segments;
					kicad_segment_cursor := kicad_segments.first;

					-- loop in segments of current strand
					-- A kicad net segment has labels and junctions.
					log_indentation_up;
					while kicad_segment_cursor /= et_kicad.type_net_segments.no_element loop

						log ("segment" & et_schematic.to_string (
							segment		=> element (kicad_segment_cursor),
							scope		=> et_coordinates.XY),
							log_threshold + 4);
						
						-- get coordinates and junctions from the current kicad net segment:
						net_segment_base := et_schematic.type_net_segment_base (element (kicad_segment_cursor));

						-- get labels from current kicad net segment
						net_labels_native := tag_and_simple_labels (element (kicad_segment_cursor));

						-- read net junctions of the current segment in temp. collection net_junctions_native
						net_junctions_native := read_net_junctions (element (kicad_segment_cursor));
						
						-- Collect native net segment in list net_segments_native.
						-- Native net segments have labels and junctions.
						et_schematic.type_net_segments.append (
							container	=> net_segments_native,
							new_item	=> (net_segment_base with 
											labels		=> net_labels_native,
											junctions	=> net_junctions_native)
							);

						next (kicad_segment_cursor);
					end loop;
					log_indentation_down;

					-- get lowest x/y coordinates of current kicad strand:
					strand_base := et_schematic.type_strand_base (element (kicad_strand_cursor));

					-- collect native strand (incl. segments) in list strands_native
					et_schematic.type_strands.append (
						container	=> strands_native,
						new_item	=> (strand_base with net_segments_native));

					-- clear collection of net segments (for the next strand)
					clear (net_segments_native);
					
					next (kicad_strand_cursor);
				end loop;

				net.strands := strands_native;

				log_indentation_down;
			end insert_strands;

			procedure copy_layout_stuff (
				net_name	: in et_schematic.type_net_name.bounded_string;
				net			: in out et_schematic.type_net) is
			begin -- copy_layout_stuff
				log_indentation_up;

				log ("class" & et_pcb.to_string (element (kicad_net_cursor).class), log_threshold + 3);
				net.class := element (kicad_net_cursor).class;	
				
				log ("tracks, vias, polygons ...", log_threshold + 3);
				net.route := element (kicad_net_cursor).route;
				-- CS: mirror y coordinates
				-- CS log details on tracks, vias, ...
				
				log_indentation_down;
			end copy_layout_stuff;
											
		begin -- copy_nets
			-- loop in kicad nets
			while kicad_net_cursor /= et_kicad.type_nets.no_element loop
				log ("net " & et_schematic.to_string (key (kicad_net_cursor)), log_threshold + 2);

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
		
	begin -- to_native
		flatten (log_threshold); -- levels all paths so that we get a real flat design

		log ("converting ...", log_threshold);
		log_indentation_up;
		
		while module_cursor_kicad /= et_kicad.type_rig.no_element loop
			log ("module " & et_coordinates.to_string (key (module_cursor_kicad)), log_threshold + 1);
			log_indentation_up;

			-- create an empty module
			et_schematic.type_rig.insert (
				container	=> et_schematic.rig,
				key			=> key (module_cursor_kicad),
				position	=> module_cursor_native,
				inserted	=> module_inserted -- should always be true
				);

			-- copy general stuff (notes, routing info, silk screen, documentation, net class settings, ...)
			update_element (
				container	=> et_schematic.rig,
				position	=> module_cursor_native,
				process		=> copy_general_stuff'access);

			-- copy components (incl. their units and positions in layout)
			update_element (
				container	=> et_schematic.rig,
				position	=> module_cursor_native,
				process		=> copy_components'access);

			-- copy nets (incl. layout related stuff: tracks, vias, polygons, net classes, ...)
			update_element (
				container	=> et_schematic.rig,
				position	=> module_cursor_native,
				process		=> copy_nets'access);

			-- CS copy frames, mirror y
			
			log_indentation_down;

			next (module_cursor_kicad);
		end loop;


-- 		log ("packages ...", log_threshold);




		log_indentation_down;
		

		
	end to_native;
	

	
end et_kicad_to_native;

-- Soli Deo Gloria
