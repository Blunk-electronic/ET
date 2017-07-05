------------------------------------------------------------------------------
--                                                                          --
--                           SYSTEM ET KICAD                                --
--                                                                          --
--                                 ET                                       --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
--         Copyright (C) 2017 Mario Blunk, Blunk electronic                 --
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

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--

with ada.characters;			use ada.characters;
with ada.characters.latin_1;	use ada.characters.latin_1;
--with ada.characters.handling;	use ada.characters.handling;
with ada.text_io;				use ada.text_io;
with ada.strings; 				use ada.strings;
with ada.strings.fixed; 		use ada.strings.fixed;
with ada.strings.bounded; 		use ada.strings.bounded;
with ada.strings.unbounded; 	use ada.strings.unbounded;
with ada.numerics.real_arrays;  use ada.numerics.real_arrays;
with ada.directories;			use ada.directories;
with ada.exceptions; 			use ada.exceptions;

with et_schematic;				use et_schematic;

with et_geometry;				use et_geometry;

with et_general;				use et_general;

package body et_kicad is


	procedure import_design is
		use et_import.type_schematic_file_name;
		
		function read_project_file return et_import.type_schematic_file_name.bounded_string is
			-- 			name_of_project_file : unbounded_string;
			use et_import.type_project_file_name;
			
			max_length_of_line_of_project_file : constant positive := 1000; -- CS: should suffice for now, increase if neccessary
			package type_line_of_project_file is new generic_bounded_length(max_length_of_line_of_project_file);
			use type_line_of_project_file;
            line_of_project_file : type_line_of_project_file.bounded_string;
            line_counter : natural := 0;
            section_eeschema_entered : boolean := false;
            section_eeschema_libraries_entered : boolean := false;            

            procedure clear_section_entered_flags is
            begin
                section_eeschema_entered := false;
                section_eeschema_libraries_entered := false;
            end clear_section_entered_flags;
            
		begin
-- 			name_of_project_file:= to_unbounded_string( project & '.' & file_extension_project);
-- 			if exists(to_string(name_of_project_file)) then

			put_line("reading project file ...");

				open (file => et_import.project_file_handle, mode => in_file, name => to_string(et_import.project_file_name));
				set_input (et_import.project_file_handle);
                while not end_of_file loop
                    line_counter := line_counter + 1;
                    line_of_project_file := to_bounded_string(get_line);

                    -- test header [eeschema]
                    if get_field(text_in => to_string(line_of_project_file), position => 1) = project_header_eeschema then
                        clear_section_entered_flags;
                        section_eeschema_entered := true;
                    end if;

                    -- test header [eeschema/libraries]
                    if get_field(text_in => to_string(line_of_project_file), position => 1) = project_header_eeschema_libraries then
                        clear_section_entered_flags;
                        section_eeschema_libraries_entered := true;
                    end if;

                    
                    if section_eeschema_entered then
                        put_line(" " & to_string(line_of_project_file));
                    end if;
                    
				end loop;
				close ( et_import.project_file_handle );
-- 			else
-- 				put_line(message_warning & "Project file '" & to_string(name_of_project_file) & "' not found !");
-- 			end if;

			-- Derive the schematic file name from the project file. It is just a matter of file extension.
			return et_import.type_schematic_file_name.to_bounded_string(
				compose(name => base_name(to_string(project_file_name)), 
						extension => file_extension_schematic));
		end read_project_file;

        -- While reading submodules (sheets) the path_to_submodule keeps record of current point in the design 
        -- hierarchy. Each time a submodule A has been found with nested submodules, the name of A is appended here.
        -- Once the parent module is entered again, the name A is removed from the list. When assigning coordinates
        -- to an object, the path_to_submodule is read. 
        -- So by concatenation of the module names of this list (from first to last) we get a full path that tells us
        -- the exact location of the module withing the design hierarchy.
        path_to_submodule : type_path_to_submodule.list;

        -- Sometimes we need to output the location of a submodule:
        procedure write_path_to_submodule is
            c : type_path_to_submodule.cursor;            
        begin
            put("path/location: ");

            c := type_path_to_submodule.first(path_to_submodule);            

			-- If there is a hierarchy deeper than 1, write path to submodule:
            if type_path_to_submodule.length(path_to_submodule) > 1 then
                for n in 1..type_path_to_submodule.length(path_to_submodule)-1 loop
                    put(type_submodule_name.to_string(type_path_to_submodule.element(c)) & ".");
                    c := type_path_to_submodule.next(c);
                end loop;
            
                c := type_path_to_submodule.last(path_to_submodule);

				-- write the submodule name
                put(type_submodule_name.to_string(type_path_to_submodule.element(c)));
			else
				-- no hierarchy. write just the submodule name
                put(type_submodule_name.to_string(type_path_to_submodule.element(c)));
            end if;
            
            new_line;            
		end write_path_to_submodule;
		
        -- Here we append a submodule name the the path_to_submodule.
        procedure append_name_of_parent_module_to_path(submodule : in type_submodule_name.bounded_string) is
        begin
            -- Since we are dealing with file names, the extension must be removed before appending.            
            type_path_to_submodule.append(path_to_submodule,
                type_submodule_name.to_bounded_string(base_name(type_submodule_name.to_string(submodule)))
                );
        end append_name_of_parent_module_to_path;

        -- Here we remove the last submodule name form the path_to_submodule.
        procedure delete_last_module_name_from_path is
        begin
            type_path_to_submodule.delete_last(path_to_submodule);
        end delete_last_module_name_from_path;


        
		function read_schematic (name_of_schematic_file : in et_import.type_schematic_file_name.bounded_string) 
			return type_list_of_submodule_names_extended is
		-- Reads the given schematic file. If it contains submodules (hierarchic sheets), 
        -- they will be returned in a list_of_submodules. Otherwise the returned list is empty.
        
			list_of_submodules : type_list_of_submodule_names_extended; -- list to be returned
			name_of_submodule_scratch : type_submodule_name.bounded_string; -- temporarily used before appended to list_of_submodules
       
			max_length_of_line_of_schematic_file : positive := 1000; -- CS: should suffice for now, increase if neccessary
			package type_line_of_schematic_file is new generic_bounded_length(max_length_of_line_of_schematic_file);
			use type_line_of_schematic_file;
			line_of_schematic_file : type_line_of_schematic_file.bounded_string;
			line_counter : natural := 0;
			sheet_file : type_sheet_file.bounded_string;
			sheet_count_total, sheet_number_current : positive;
			net_segment_entered : boolean := false;

            -- When reading the sheet headers, their content is held temporarily in scratch variables
            sheet_header_scratch : type_sheet_header; -- The sheet header before being appended to list of sheet headers.
            --list_of_libraries_scratch : type_list_of_library_names.vector; -- Library names before
            schematic_headline_processed : boolean := false;
            
            
            -- When reading net labels, they are held temporarily in scratch variables, 
            -- then added to wild lists of labels for later sorting:
			simple_label_entered : boolean := false;			
			label_simple_scratch: type_net_label_simple;
			wild_simple_label_collection_scratch : type_list_of_labels_simple.vector;
			tag_label_entered : boolean := false;
			label_tag_scratch: type_net_label_tag;
            wild_tag_label_collection_scratch : type_list_of_labels_tag.vector;

            -- When reading notes, they are held temporarily in scratch variables,
            -- then added to the list of notes.
            note_entered : boolean := false;
            note_scratch : type_note;
			
			function to_orientation (text_in : in string) return type_orientation is
			-- Converts the kicad notation of a label orientation to degrees.
				o_in : type_label_orientation := type_label_orientation'value(text_in);
				o_out : type_orientation;
			begin
				case o_in is
					when 0 => o_out := deg_180;
					when 1 => o_out := deg_90;
					when 2 => o_out := deg_0;
					when 3 => o_out := deg_270;
				end case;
				return o_out;
				-- CS: exception handler
			end to_orientation;

			function to_direction (text_in : in string) return type_label_direction is
			-- Converts the direction of a label to a type_label_direction. 
			-- CS: currently case sensitive !
				d_out : type_label_direction := input;
			begin
				if text_in = schematic_keyword_label_dir_input then
					d_out := input;
				elsif text_in = schematic_keyword_label_dir_output then
					d_out := output;
				elsif text_in = schematic_keyword_label_dir_bidir then
					d_out := bidir;
				elsif text_in = schematic_keyword_label_dir_tristate then
					d_out := tristate;
				elsif text_in = schematic_keyword_label_dir_passive then
					d_out := passive;
				else
					put_line(et_import.report_handle, message_error & "Label direction unknown !");
					raise constraint_error;
				end if;
				
				return d_out;
			end to_direction;

            function to_text_attributes ( size : in type_text_size; style : in string; width : type_text_line_width) 
            -- Converts given text size, style and line width to a type_text_attributes.
            -- CS: currently case sensitive !                
                return type_text_attributes is
                a_out : type_text_attributes;
            begin
                a_out.size := size;
                a_out.width := width;
                
                if style = schematic_style_normal then
                    if width > 0 then
                        a_out.style := bold;
                    else
                        a_out.style := default;
                    end if;
                    
                elsif style = schematic_style_italic then
                    if width > 0 then
                        a_out.style := bold_italic;
                    else
                        a_out.style := italic;
                    end if;

                else
                    write_message(
                        file_handle => et_import.report_handle,
                        text => "Text font unknown !",
                        console => true
                        );
					raise constraint_error;
                end if;
                
                return a_out;
            end to_text_attributes;

            function to_visible ( text : in string) return boolean is
            -- Converts the kicad text visible status to boolean type.
                visible : boolean;
            begin
                if text = schematic_text_visible then
                    visible := true;
                elsif text = schematic_text_invisible then
                    visible := false;
                end if;
                return visible;
            end to_visible;

            function to_alignment_horizontal ( text : in string) return type_text_alignment_horizontal is
            -- Converts a horizontal kicad text alignment to type_text_alignment_horizontal.
                a : type_text_alignment_horizontal;
            begin
                case type_schematic_text_alignment_horizontal'value(text) is
                    when L => a := left;
                    when C => a := center;
                    when R => a := right;
                end case;
                return a;
            end to_alignment_horizontal;

            function to_alignment_vertical ( text : in string) return type_text_alignment_vertical is
            -- Converts a vertical kicad text alignment to type_text_alignment_vertical.
                a : type_text_alignment_vertical;
            begin
                case type_schematic_text_alignment_vertical'value(text) is
                    when TNN => a := top;
                    when CNN => a := center;
                    when BNN => a := bottom;
                end case;
                return a;
            end to_alignment_vertical;

			-- In the first stage, all net segments of this sheet go into a wild collection of segments.
			-- Later they will be sorted and connected by their coordinates (start and and points)
			segment_count : natural; -- holds the total number of segments within a sheet
			seg : natural; -- points to the segment being processed
			type type_segment_side is (start_point, end_point ); -- the end point of a segment
			
			type type_wild_net_segment is new type_net_segment with record
				s, e : boolean := false; -- flag indicates the end point beeing assumed
				picked : boolean := false; -- flag indicates that the segment has been added to the anonymous net
			end record;
			segment_scratch: type_wild_net_segment; -- temporarily used when reading net segments into a wild collecton of segments
			
			package type_wild_list_of_net_segments is new vectors ( 
				index_type => positive,  -- every net segment has an id
				element_type => type_wild_net_segment);
			wild_segment_collection : type_wild_list_of_net_segments.vector;

			junction_count : natural := 0;
			junction_scratch : type_net_junction; -- temporarily used when reading net junctions into a wild collection of junctions
			wild_collection_of_junctions : type_list_of_net_junctions.vector;

			function junction_sits_on_segment(junction : in type_net_junction; segment : in type_wild_net_segment) return boolean is
			-- Returns true if the given junction sits on the given net segment.
				point 		: type_coordinates := junction.coordinates;
				line_start 	: type_coordinates := segment.coordinates_start;
				line_end 	: type_coordinates := segment.coordinates_end;
				zero 		: constant type_grid := 0.0;
				sits_on_segment : boolean := false;
				d : type_distance_point_from_line;
			begin
				-- calculate the shortes distance of point from line.
				d := distance_of_point_from_line (point => point, line_start => line_start, line_end => line_end,
					line_range => inside_end_points);
				if (not d.out_of_range) and d.distance = zero then
 					sits_on_segment := true;
 				end if;
				return sits_on_segment;
			end junction_sits_on_segment;
			
			-- Prodcedures that set the s,e or picked flag. These procedures are called via access.
			procedure set_e ( segment : in out type_wild_net_segment ) is begin segment.e := true; end set_e;
			procedure set_s ( segment : in out type_wild_net_segment ) is begin segment.s := true; end set_s;
			procedure set_picked ( segment : in out type_wild_net_segment ) is begin segment.picked := true; end set_picked;

			procedure write_coordinates_of_label ( label : in type_net_label) is
			begin
				case label.label_appearance is
					when simple =>
						write_message(
							file_handle => et_import.report_handle,
							text => "simple label: ",
							lf => false,
							identation => 2);
					when tag =>
						write_message(
							file_handle => et_import.report_handle,
							text => "tag label: ",
							lf => false,
							identation => 3);

						-- CS: directon, global, hierarchic
				end case;

				put_line(et_import.report_handle, "'" & type_net_name.to_string(label.text) & "' at position (x/y/sheet)" &
						type_grid'image(label.coordinates.x) & "/" & trim(type_grid'image(label.coordinates.y),left) & "/" &
						trim(positive'image(label.coordinates.sheet_number),left)
						);
			end write_coordinates_of_label;
			
			procedure write_coordinates_of_segment (segment : in type_net_segment) is
			begin
				write_message(
					file_handle => et_import.report_handle,
					text => "start " & 
						trim (type_grid'image( segment.coordinates_start.x),left) & "/" &
						trim (type_grid'image( segment.coordinates_start.y),left) &
						" end " &
						trim (type_grid'image( segment.coordinates_end.x),left) & "/" &
						trim (type_grid'image( segment.coordinates_end.y),left),
					identation => 4);
					
				-- CS: write sheet number ?
				--new_line(et_import.report_handle);
			end write_coordinates_of_segment;

			procedure write_coordinates_of_junction (junction : in type_net_junction) is
			begin
				write_message(
					file_handle => et_import.report_handle,
					text => "position (x/y/sheet) " & 
						trim(type_grid'image(junction.coordinates.x),left) & "/" &
						trim(type_grid'image(junction.coordinates.y),left) & "/" &
						trim(positive'image(junction.coordinates.sheet_number),left),
					identation => 3);
			end write_coordinates_of_junction;			
			
			procedure write_coordinates_of_device_block (block : in type_device_block) is
			begin
				write_message (
					file_handle => et_import.report_handle,
					text => " position (x/y/sheet) " & 
						trim(type_grid'image(block.coordinates.x),left) & "/" &
						trim(type_grid'image(block.coordinates.y),left) & "/" &
						trim(positive'image(block.coordinates.sheet_number),left)
						);
			end write_coordinates_of_device_block;			
			
			-- An anonymous_net is a list of net segments that are connected with each other (by their start or end points).
			-- The anonymous net gets step by step more properties specified: name, scope and some status flags:
			package type_anonymous_net is new vectors (
				index_type => positive,  -- every net segment has an id
				element_type => type_net_segment);
			type type_anonymous_net_extended is record
				segments 	: type_anonymous_net.vector;	-- the list of segments
				name 		: type_net_name.bounded_string; -- the name (derived from net labels)
				scope 		: type_scope_of_net := local;	-- the scope (derived from net labels)
				processed	: boolean := false;				-- set once a label has been found on the net
				sorted		: boolean := false;				-- set once sorted out while sorting named nets
			end record;
			-- When sorting named nets, this procedure sets the "sorted" flag of the anonymous net.
			procedure set_sorted ( anon_net : in out type_anonymous_net_extended ) is begin anon_net.sorted := true; end set_sorted;
			anonymous_net : type_anonymous_net_extended;
			
			procedure add_segment_to_anonymous_net ( id : in positive ) is
			-- Adds a net segment (indicated by id) to a list of segments connected with each other.
			-- This procedure happens to be called for a certain segment more than once (unavoidable). So the flag "picked" serves
			-- as indicator for a segment already added to the anonymous net.
				scratch : type_net_segment;
			begin
				-- If segment already picked and added to anonymous net, do nothing with this segment. Otherwise set the "picked" flag
				-- of that segment, output the coordinates of the segment, add it to anonymous net.
				if type_wild_list_of_net_segments.element(wild_segment_collection,id).picked then
					null;
					--put("  picked");
				else
					--put(et_import.report_handle,"  segment" & positive'image(id) & ":");

-- 					write_message(
-- 						file_handle => et_import.report_handle,
-- 						lf => false,
-- 						text => "segment" & positive'image(id) & ":",
-- 						identation => 3);
					
					--put("  segment" & positive'image(id) & ":");
					type_wild_list_of_net_segments.update_element(
							container => wild_segment_collection,
							index => id,
							process => set_picked'access);

					write_coordinates_of_segment(segment => type_net_segment(type_wild_list_of_net_segments.element(wild_segment_collection,id)));
					
					scratch := type_net_segment(type_wild_list_of_net_segments.element(wild_segment_collection,id));
					type_anonymous_net.append(anonymous_net.segments,scratch);
				end if;
			end add_segment_to_anonymous_net;

			-- The function search_for_same_coordinates returns this type:
			type type_same_coord_result is record
				valid : boolean; -- indicates that a segment with matching coordinates has been found. When false, no segment found -> consider id and side invalid
				id : positive; -- id of the segment found
				side : type_segment_side; -- end point of the segment found
			end record;
			same_coord_result : type_same_coord_result;
			side_scratch : type_segment_side;
			
			function search_for_same_coordinates (id : in positive; seg_in : in type_wild_net_segment; side : in type_segment_side) return type_same_coord_result is
			-- Starting from a segment indicated by id and the end point (given by side), 
			-- search in wild_segment_collection for a segment with matching start or end point.
			-- In general untouched segments are preferred in the search. Half processed segments are of secondary relevance.
			-- Once a suitable segment was found, sc is assigned with neccessary data to be returned to the parent unit. The search for 
			-- a suitable segment excludes fully processed segments and the given segment (id).
				sc : type_same_coord_result;
				line_start, line_end : type_coordinates;
				s, e	: boolean; -- indicate the end point, that has been processed already
				untouched, half_processed : boolean; -- indicate whether a segment is completely untouched or processed in only one direction
			begin
				-- Set E/S flag:
				-- If we start the search from the end_point of a segment, the e-flag is to be set. This indicates the end_point has been processed.
				-- If we start the search from the start_point of a segment, the s-flag is to be set. This indicates the start_point has been processed.				
				case side is
					when end_point =>
						--put_line(et_import.report_handle,"  --> origin of search (END): " & type_grid'image(seg_in.coordinates_end.x) & "/" & type_grid'image(seg_in.coordinates_end.y));
						type_wild_list_of_net_segments.update_element(
								container => wild_segment_collection,
								index => id,
								process => set_e'access);
					when start_point =>
						--put_line(et_import.report_handle,"  --> origin of search (START): " & type_grid'image(seg_in.coordinates_start.x) & "/" & type_grid'image(seg_in.coordinates_start.y));
						type_wild_list_of_net_segments.update_element(
								container => wild_segment_collection,
								index => id,
								process => set_s'access);
				end case;

				-- First, search completely untouched segments (they have both e and s flag cleared).
				-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
				-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
				-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
				for i in 1..segment_count loop
					if i /= id then -- skip the given segment
						line_start := type_wild_list_of_net_segments.element(wild_segment_collection,i).coordinates_start;
						line_end   := type_wild_list_of_net_segments.element(wild_segment_collection,i).coordinates_end;
						s  := type_wild_list_of_net_segments.element(wild_segment_collection,i).s;
						e  := type_wild_list_of_net_segments.element(wild_segment_collection,i).e;
						untouched := not (s or e); -- neither s nor e set
						--fully_processed := s and e;

						if untouched then 
							--put(et_import.report_handle,"probe untouched segment: ");
							--write_coordinates_of_segment(type_net_segment(type_wild_list_of_net_segments.element(wild_segment_collection,i)));
							
							case side is
								-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
								when end_point =>
									if line_start.x = seg_in.coordinates_end.x and line_start.y = seg_in.coordinates_end.y then
										sc.valid := true;
										sc.side := start_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

									if line_end.x = seg_in.coordinates_end.x and line_end.y = seg_in.coordinates_end.y then
										sc.valid := true;
										sc.side := end_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

								-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.									
								when start_point =>
									if line_start.x = seg_in.coordinates_start.x and line_start.y = seg_in.coordinates_start.y then
										sc.valid := true;
										sc.side := start_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

									if line_end.x = seg_in.coordinates_start.x and line_end.y = seg_in.coordinates_start.y then
										sc.valid := true;
										sc.side := end_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;
							end case;
						end if;
					end if;
				end loop;

				-- No untouched segment found.
				-- Now, search half_processed segments (they have either e or s flag (BUT NOT BOTH AT THE SAME TIME!) set).
				-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
				-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
				-- If suitable segment found, exit and return its ID and a the "valid"-flag set.
				for i in 1..segment_count loop
					if i /= id then -- skip the given segment
						line_start := type_wild_list_of_net_segments.element(wild_segment_collection,i).coordinates_start;
						line_end   := type_wild_list_of_net_segments.element(wild_segment_collection,i).coordinates_end;
						s  := type_wild_list_of_net_segments.element(wild_segment_collection,i).s;
						e  := type_wild_list_of_net_segments.element(wild_segment_collection,i).e;
						half_processed := s xor e;

						if half_processed then
							--put(et_import.report_handle,"probe half-processed segment: ");
							--write_coordinates_of_segment(type_net_segment(type_wild_list_of_net_segments.element(wild_segment_collection,i)));
							
							case side is
								-- If the search starts from the end_point of the given net, find a segment whose start or end point matches.
								when end_point =>
									if line_start.x = seg_in.coordinates_end.x and line_start.y = seg_in.coordinates_end.y then
										sc.valid := true;
										sc.side := start_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

									if line_end.x = seg_in.coordinates_end.x and line_end.y = seg_in.coordinates_end.y then
										sc.valid := true;
										sc.side := end_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

								-- If the search starts from the start_point of the given net, find a segment whose start or end point matches.
								when start_point =>
									if line_start.x = seg_in.coordinates_start.x and line_start.y = seg_in.coordinates_start.y then
										sc.valid := true;
										sc.side := start_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;

									if line_end.x = seg_in.coordinates_start.x and line_end.y = seg_in.coordinates_start.y then
										sc.valid := true;
										sc.side := end_point;
										sc.id := i;
										goto matching_segment_coordinates_found;
									end if;
							end case;
						end if;
					end if;
				end loop;
				
				sc.valid := false;
				sc.id := id;
				return sc;
			<<matching_segment_coordinates_found>>
				add_segment_to_anonymous_net(sc.id);
				--put_line(et_import.report_handle, "match");
				return sc;
			end search_for_same_coordinates;
			
			-- A list of anonymous nets:
			package type_list_of_anonymous_nets is new vectors (
				index_type => positive,  -- every anonymous net has an id
				element_type => type_anonymous_net_extended
				);
			-- The procedure add_net_to_list_of_anonymous_nets uses this container for temporarily storage of anonymous nets.
			list_of_anonymous_nets : type_list_of_anonymous_nets.vector; 
			
			procedure add_net_to_list_of_anonymous_nets is
			-- Once an anonymous net is complete, it gets appended to a list of anonymous nets. 
			-- Afterward the anonymous net is deleted. It is a vector of net segments which must be purged so that the vector
			-- "anonymous_net" can be filled with net segments of the next anonymous net.
			begin
				type_list_of_anonymous_nets.append(list_of_anonymous_nets,anonymous_net);
				type_anonymous_net.delete(anonymous_net.segments,index => 1, count => type_anonymous_net.length(anonymous_net.segments));
			end add_net_to_list_of_anonymous_nets;

			procedure associate_net_labels_with_anonymous_nets is
			-- All anonymous nets must be given a name. The name is enforced by the a net label. The first label found on the net sets the net name.
			-- Other labels on the net are checke for their name only. If the name differs from the net name set earlier, a warning is output.
			-- Nets without label remain anonymous by using the notation "N$"
				ls  :	type_net_label_simple;
				lt  : 	type_net_label_tag;				
				a,b : 	type_anonymous_net_extended;
				s   : 	type_net_segment;
				lls : 	type_list_of_labels_simple.vector;
				llt : 	type_list_of_labels_tag.vector;				
				net_scratch : type_net;
				
				function label_sits_on_segment (label : in type_net_label; segment : in type_net_segment) return boolean is
					point 		: type_coordinates := label.coordinates;
					line_start 	: type_coordinates := segment.coordinates_start;
					line_end 	: type_coordinates := segment.coordinates_end;
					zero 		: constant type_grid := 0.0;
					sits_on_segment : boolean := false;
					d : type_distance_point_from_line;
				begin
					-- calculate the shortes distance of point from line.
					d := distance_of_point_from_line (point => point, line_start => line_start, line_end => line_end,
						line_range => with_end_points);
					--put_line(et_import.report_handle,"distance: " & type_grid'image(d.distance));
					if not d.out_of_range and d.distance = zero then
						sits_on_segment := true;
					end if;
					return sits_on_segment;
				end label_sits_on_segment;
				
			begin -- associate_net_labels_with_anonymous_nets
				-- This does only make sense if there are nets at all:
				if type_list_of_anonymous_nets.length(list_of_anonymous_nets) > 0 then
					--put_line(et_import.report_handle,"associating net labels with nets ...");
					write_message(
						file_handle => et_import.report_handle,
						text => "associating net labels with nets ...",
						identation => 2);
					
					-- Loop in list of anonymous nets, get a (non-processed-yet) net, loop in list of segments and find a (non-processed-yet)
					-- net label that sits on the net segment. If label sits on segment:
					--  - assume label text as name of net (and check other labels of the anonymous net)
					--
					--  - mark label as processed
					--  - update/replace label in wild_simple_label_collection_scratch or wild_tag_label_collection_scratch
					--
					--  - Collect label in temporarily list of labels.
					--
					--  - Mark anonymous net as processed. This indicates that the net has a name (given by a label).
					--    Non-Processed nets are those without a label.
					--  - update/replace anonymous net in list_of_anonymous_nets
					for n in 1..type_list_of_anonymous_nets.length(list_of_anonymous_nets) loop
						a := type_list_of_anonymous_nets.element(list_of_anonymous_nets, positive(n)); -- get anonymous net
						--put_line(et_import.report_handle,"anonymous net #" & trim(count_type'image(n),left) & ": ");
						if not a.processed then
							for b in 1..type_anonymous_net.length(a.segments) loop -- loop for each segment of anonymous_net
								s := type_anonymous_net.element(a.segments, positive(b)); -- get segment

								--put(et_import.report_handle, "segment: "); write_coordinates_of_segment(s);
								
								-- Loop in list of simple labels:
								if type_list_of_labels_simple.length(wild_simple_label_collection_scratch) > 0 then -- do that if there are simple labels at all
									--put_line(" simple labels ...");
									for l in 1..type_list_of_labels_simple.length(wild_simple_label_collection_scratch) loop 
										ls := type_list_of_labels_simple.element(wild_simple_label_collection_scratch, positive(l)); -- get simple label
										if not ls.processed then
											--put(et_import.report_handle, "   probing "); write_coordinates_of_label( type_net_label(ls));
											if label_sits_on_segment(label => type_net_label(ls), segment => s) then

												--put(et_import.report_handle, "match: "); write_coordinates_of_label( type_net_label(ls));

												-- The first matching label dictates the net name. 
												-- If other labels with text differing from net name found, output warning.
												if type_net_name.length(a.name) = 0 then -- If this is the first matching label
													a.name := ls.text; -- assume the label text as net name.
												else
													-- If label text is different from previously assigned net name:
													if type_net_name.to_string(a.name) /= type_net_name.to_string(ls.text) then 
														put_line(et_import.report_handle,message_warning & "Net '" & type_net_name.to_string(a.name) &
														"' has contradicting label '" & type_net_name.to_string(ls.text) & "'");
													end if;
												end if;

												-- mark simple label as processed and update/replace it in wild_simple_label_collection_scratch
												ls.processed := true;
												type_list_of_labels_simple.replace_element(
													container => wild_simple_label_collection_scratch,
													index => positive(l),
													new_item => ls);

												-- Collect simple label (ls) in temporarily list of simple labels (lls).
												type_list_of_labels_simple.append(lls,ls);

												-- Mark anonymous net as processed.												
												a.processed := true;
											end if;
										end if;
									end loop;

									-- Copy list of simple labels (lls) to current segment (s).
									s.label_list_simple := lls;
									-- Update/replace segment in current anonymous net.
									type_anonymous_net.replace_element(
										container => a.segments, -- the list of segments of the current anonymous net
										index => positive(b), -- the segment id
										new_item => s); -- the updated segment
									-- Clean up: Purge temporarily list of simple labels for next spin.
									type_list_of_labels_simple.delete(container => lls, index => 1, count => type_list_of_labels_simple.length(lls));

									-- Update/replace anonymous net in list_of_anonymous_nets.
									type_list_of_anonymous_nets.replace_element(
										container => list_of_anonymous_nets, -- the list of anonymous nets
										index => positive(n), -- the anonymous net id
										new_item => a); -- the updated anonymous net
								end if;
								
								-- Loop in list of tag labels:
								if type_list_of_labels_tag.length(wild_tag_label_collection_scratch) > 0 then -- do that if there are tag labels at all
									--put_line(" hierarchic and global labels ...");									
									for l in 1..type_list_of_labels_tag.length(wild_tag_label_collection_scratch) loop 
										lt := type_list_of_labels_tag.element(wild_tag_label_collection_scratch, positive(l)); -- get tag label
										if not lt.processed then								
											if label_sits_on_segment(label => type_net_label(lt), segment => s) then

-- 												put_line(et_import.report_handle," tag label: " & type_net_name.to_string(lt.text) & " position:" &
-- 												type_grid'image(lt.coordinates.x) & "/" &
-- 												trim(type_grid'image(lt.coordinates.y),left));

												write_coordinates_of_label(type_net_label(lt));
-- 												write_message(
-- 													file_handle => et_import.report_handle,
-- 													text => "tag label: " & type_net_name.to_string(lt.text) & " position:" &
-- 															type_grid'image(lt.coordinates.x) & "/" &
-- 															trim(type_grid'image(lt.coordinates.y),left),
-- 													identation => 3);

												-- The first matching label dictates the net name and scope. 
												-- If other labels with text differing from net name found, output warning.
												if type_net_name.length(a.name) = 0 then -- If this is the first matching label
													a.name := lt.text; -- assume the label text as net name.
													if lt.global then 
														a.scope := global;
													end if;
													if lt.hierarchic then 
														a.scope := hierarchic;
													end if;
												else
													-- If label text is different from previously assigned net name:
													if type_net_name.to_string(a.name) /= type_net_name.to_string(lt.text) then 
														put_line(et_import.report_handle,message_warning & "Net '" & type_net_name.to_string(a.name) &
														"' has contradicting label '" & type_net_name.to_string(lt.text) & "'");
													end if;

													-- CS: check for contradicting scope
												end if;

												-- mark tag label as processed and update/replace it in wild_tag_label_collection_scratch
												lt.processed := true;
												type_list_of_labels_tag.replace_element(
													container => wild_tag_label_collection_scratch,
													index => positive(l),
													new_item => lt);

												-- Collect tag label (lt) in temporarily list of simple labels (llt).
												type_list_of_labels_tag.append(llt,lt);

												-- Mark anonymous net as processed.												
												a.processed := true;
											end if;
										end if;
									end loop;

									-- Copy list of tag labels (llt) to current segment (s).
									s.label_list_tag := llt;
									-- Update/replace segment in current anonymous net.
									type_anonymous_net.replace_element(
										container => a.segments, -- the list of segments of the current anonymous net
										index => positive(b), -- the segment id
										new_item => s); -- the updated segment
									-- Clean up: Purge temporarily list of tag labels for next spin.
									type_list_of_labels_tag.delete(container => llt, index => 1, count => type_list_of_labels_tag.length(llt));

									-- Update/replace anonymous net in list_of_anonymous_nets.
									type_list_of_anonymous_nets.replace_element(
										container => list_of_anonymous_nets, -- the list of anonymous nets
										index => positive(n), -- the anonymous net id
										new_item => a); -- the updated anonymous net
								end if;
							end loop;
						end if;
					end loop;

					-- Sort anonymous nets without label.
					-- Anonymous nets without label have no name -> "processed" flag is still cleared.
					-- As placeholder for the name we use the notation "N$" where n is an index (derived from the element id in list_of_anonymous_nets)
					-- Their scope is strictly "local".
					-- We us an intermediate variable net_scratch for transfer to the module netlist.
					write_message(
						file_handle => et_import.report_handle,
						text => "sorting name-less nets ...",
						identation => 2);
					
					for n in 1..type_list_of_anonymous_nets.length(list_of_anonymous_nets) loop
						a := type_list_of_anonymous_nets.element(list_of_anonymous_nets, positive(n)); -- get anonymous net
						if not a.processed then

							-- build temporarily net
							net_scratch.name := type_net_name.to_bounded_string("N$" & trim(count_type'image(n),left));
							write_message(
								file_handle => et_import.report_handle,
								text => type_net_name.to_string(net_scratch.name),
								identation => 3);
							
							net_scratch.scope := local;

							-- append segments to net_scratch
							for b in 1..type_anonymous_net.length(a.segments) loop -- loop for each segment of anonymous_net a
								s := type_anonymous_net.element(a.segments, positive(b)); -- get segment
								type_list_of_net_segments.append(container => net_scratch.segments, new_item => s);
								write_coordinates_of_segment(segment => s);
							end loop;

                            -- assign coordinates
                            net_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                            net_scratch.coordinates.path := path_to_submodule;
                            -- CS: x,y coordinates should be the lowest available on the first sheet.
                            -- CS: do not assign sheet and x/y at all ?
                            -- net_scratch.coordinates.sheet := sheet_number_current;
                            
							-- append net_scratch to module netlist, then purge net_scratch.segments for next spin
							type_net_list_of_module.append(container => module.nets, new_item => net_scratch);
							type_list_of_net_segments.delete(
								container => net_scratch.segments,
								index => 1,
								count => type_list_of_net_segments.length(net_scratch.segments));
						end if;
					end loop;

					-- Sort anonymous nets with label.
					write_message(
						file_handle => et_import.report_handle,
						text => "sorting named nets ...",
						identation => 2);
					
					for n in 1..type_list_of_anonymous_nets.length(list_of_anonymous_nets) loop
						a := type_list_of_anonymous_nets.element(list_of_anonymous_nets, positive(n)); -- get anonymous net
						if a.processed and not a.sorted then -- if it has not been sorted yet
							--put(et_import.report_handle," " & type_net_name.to_string(a.name));
							write_message(
								file_handle => et_import.report_handle,
								text => type_net_name.to_string(a.name),
								identation => 3,
								lf => false);
							
							net_scratch.name := a.name;
							net_scratch.scope := a.scope;
							--put_line(et_import.report_handle," is " & type_scope_of_net'image(net_scratch.scope) & " with segments:");
							write_message(
								file_handle => et_import.report_handle,
								text => " is " & type_scope_of_net'image(net_scratch.scope) & " with segments:");

							-- append segments to net_scratch
							for b in 1..type_anonymous_net.length(a.segments) loop -- loop for each segment of anonymous_net a
								s := type_anonymous_net.element(a.segments, positive(b)); -- get segment
								type_list_of_net_segments.append(container => net_scratch.segments, new_item => s);
								write_coordinates_of_segment(segment => s);
							end loop;

							-- Look for other anonymous nets with the same name (a.name). Start searching from position n+1:
							-- Mark anonymous net as "sorted".
							-- If last anonymous net reached, do not look for other nets with same name.
							if n = type_list_of_anonymous_nets.length(list_of_anonymous_nets) then -- last net reached
								null; 
							else -- search for nets with same name
								for o in n+1..type_list_of_anonymous_nets.length(list_of_anonymous_nets) loop
									b := type_list_of_anonymous_nets.element(list_of_anonymous_nets, positive(o)); -- get anonymous net
									if b.processed then
										if type_net_name.to_string(b.name) = type_net_name.to_string(a.name) then

											-- CS: make sure scope of the anonymous net is the same

											-- append segments to net_scratch
											for c in 1..type_anonymous_net.length(b.segments) loop -- loop for each segment of anonymous_net b
												s := type_anonymous_net.element(b.segments, positive(c)); -- get segment
												type_list_of_net_segments.append(container => net_scratch.segments, new_item => s);
												write_coordinates_of_segment(segment => s);
											end loop;

											-- mark anonymous net as "sorted" so that the outer loop can skip it in further spins
											type_list_of_anonymous_nets.update_element(
												container => list_of_anonymous_nets, 
												index => positive(o), 
												process => set_sorted'access);
										end if;
									end if;
								end loop;
							end if;

                            -- assign coordinates
                            net_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                            net_scratch.coordinates.path := path_to_submodule;
                            -- CS: x,y coordinates should be the lowest available on the first sheet.
                            -- CS: do not assign sheet and x/y at all ?
                            -- net_scratch.coordinates.sheet := sheet_number_current;
                            
							-- append net_scratch to module netlist, then purge net_scratch.segments for next spin
							type_net_list_of_module.append(container => module.nets, new_item => net_scratch);
							type_list_of_net_segments.delete(
								container => net_scratch.segments,
								index => 1,
								count => type_list_of_net_segments.length(net_scratch.segments));

 						end if;
					end loop;
					
				else
					--put_line(et_import.report_handle,message_warning & "The schematic contains no nets to associate labels with !");
					write_message(
						file_handle => et_import.report_handle,
						text => message_warning & "The schematic does not contain nets to associate net labels with !",
						identation => 2);
				end if;
			end associate_net_labels_with_anonymous_nets;
			
			procedure process_junctions is
			-- Breaks down all net segments where a junction sits on. In the end, the number of net segments increases.
				
			-- Loops in type_wild_list_of_net_segments, tests if a junction sits on a segment.
			-- Then splits the segment where the junction sits. If there are junctions left on the remaining fragments,
			-- they will be detected in the next spin. 
			-- The flag segment_smashed indicates there are no more segments left with a junction.
				segment_scratch : type_wild_net_segment;
				junction_scratch : type_net_junction;
				
				procedure change_segment_start_coordinates ( segment : in out type_wild_net_segment) is 
				begin
					segment.coordinates_start := junction_scratch.coordinates;
				end change_segment_start_coordinates;
				
				segment_smashed : boolean := true; -- indicates whether a segment has been broken down
            begin -- process junctions
				-- Break down net segments that have a junction. Do that if the sheet has junctions at all. Otherwise skip this procedure.
				-- After breaking down net segments, the numbner of segments increases, so segment_count must be updated finally.
				if junction_count > 0 then
					write_message(
						file_handle => et_import.report_handle,
						text => "processing" & natural'image(junction_count) & " net junctions ...",
						identation => 2
						);

					-- We reason there are segments to be broken down. After smashing a segment, segment_count increases. If it
					-- does not increase anymore, all segments are processed.
					while segment_smashed loop
					
						loop_s:
						for s in 1..segment_count loop
							segment_scratch := type_wild_list_of_net_segments.element(wild_segment_collection,s); -- get a segment
							--put_line(et_import.report_handle,natural'image(s));

							-- loop in junction list until a junction has been found that sits on the segment
							for j in 1..junction_count loop 
								junction_scratch := type_list_of_net_junctions.element(wild_collection_of_junctions,j);
								if junction_sits_on_segment(junction => junction_scratch, segment => segment_scratch) then -- match

									--write_coordinates_of_segment (type_net_segment(segment_scratch));
									write_coordinates_of_junction (junction_scratch);

									-- move start coord. of the current segment to the position of the junction
									type_wild_list_of_net_segments.update_element(
										container => wild_segment_collection,
										index => s,
										process => change_segment_start_coordinates'access
										);

									-- replace end coord. of segment_scratch by pos. of junction
									segment_scratch.coordinates_end   := junction_scratch.coordinates;
									type_wild_list_of_net_segments.append(
										container => wild_segment_collection,
										new_item => segment_scratch
										);

									exit loop_s;
								end if;
							end loop;
						end loop loop_s;

						-- Test if segment_count has increased. If yes, set segment_smashed flag so that the wild_segment_collection
						-- can be searched again. Otherwise clear segment_scratch which ends this procedure.
						if natural(type_wild_list_of_net_segments.length(wild_segment_collection)) > segment_count then
							segment_smashed := true;
							-- update segment_count (should increment by 1)
							segment_count := natural(type_wild_list_of_net_segments.length(wild_segment_collection));
						else
							segment_smashed := false;							
						end if;
					end loop;
					
					write_message(
						file_handle => et_import.report_handle,
						text => "update: net segments total: " & natural'image(segment_count),
						identation => 2);
				end if;
			end process_junctions;


			description_entered : boolean := false;
            description_processed : boolean := false;
            sheet_description_entered : boolean := false;

            -- When reading the sheet descripton we need a temporarily places for storage. They will
            -- later be appended to the main module.
            drawing_frame_scratch : type_frame; -- a single drawing frame
            title_block_text_scratch : type_title_block_text; -- a single text within the title block
            list_of_title_block_texts_scratch : type_list_of_title_block_texts.vector; -- a list of title block texts
            title_block_scratch : type_title_block; -- a full title block

            -- When reading gui submodules (kicad refers to them as "sheets") they are stored temporarily here.
            -- This temporarily variable needs defaults in order to prevent misleading compiler warnings.
            submodule_gui_scratch : type_gui_submodule := (
                name => type_submodule_name.to_bounded_string(""),
                text_size_of_name => 58,
                text_size_of_file => 58,                
                coordinates => (    path => path_to_submodule,
                                    module_name => type_submodule_name.to_bounded_string( to_string(name_of_schematic_file)),
                                    sheet_number => 1,
                                    x => 0.0,
                                    y => 0.0 
                               ),
                size_x => 0.0,
                size_y => 0.0,
                timestamp => "00000000"
                );

            
			-- This is relevant for reading devices:
			device_entered : boolean := false; -- indicates that a device is being read
			device_scratch : type_device; -- temporarily used before appending a device list of the module
			device_block_scratch : type_device_block; -- temporarily used before appending a block to a device
			device_cursor_scratch : type_device_list_of_module.cursor; -- points to a device of the module
			procedure append_block ( device : in out type_device ) is 
			begin
				type_device_block_list.append(device.block_list,device_block_scratch); 
			end append_block;
			block_text_scratch : type_device_block_text;

			procedure fetch_components_from_library is
			begin
				write_message(
					file_handle => et_import.report_handle,
					text => "fetching components from libraries ...",
					identation => 2);
			end fetch_components_from_library;

        begin -- read_file_schematic_kicad
			if exists(to_string(name_of_schematic_file)) then
				put_line("reading schematic file: " & to_string(name_of_schematic_file) & " ...");
                write_path_to_submodule; -- CS: does not work. wrong position
				open (file => et_import.schematic_handle, mode => in_file, name => to_string(name_of_schematic_file));
				set_input (et_import.schematic_handle);
				while not end_of_file loop
					line_counter := line_counter + 1;
					line_of_schematic_file := to_bounded_string(get_line);
					--put_line(to_string(line_of_schematic_file));

                    -- read schematic headline like "EESchema Schematic File Version 2"
                    if not schematic_headline_processed then
                        if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_header_keyword_sys_name and
                        get_field(text_in => to_string(line_of_schematic_file), position => 2) = schematic_header_keyword_schematic and
                        get_field(text_in => to_string(line_of_schematic_file), position => 3) = schematic_header_keyword_file and
                        get_field(text_in => to_string(line_of_schematic_file), position => 4) = schematic_header_keyword_version then
                                if positive'value(get_field(text_in => to_string(line_of_schematic_file), position => 5)) = schematic_version then
                                    -- headline ok, version is supported
                                    schematic_headline_processed := true;

                                    -- Save schematic format version in temporarily sheet header:                                    
                                    sheet_header_scratch.version := positive'value(get_field(text_in => to_string(line_of_schematic_file), position => 5));
                                else
                                    write_message(
                                        file_handle => current_output,
                                        text => message_error & "schematic version" & positive'image(schematic_version) & " required.",
                                        console => true);                        
                                    raise constraint_error;
                                end if;
                        end if;
                    else

                        -- READ SHEET HEADER: stuff like:
                        --     LIBS:nucleo_core-rescue
                        --     LIBS:power
                        --     LIBS:bel_connectors_and_jumpers
                        --     LIBS:bel_primitives
                        --     LIBS:bel_stm32
                        --     LIBS:nucleo_core-cache
                        --     EELAYER 25 0
                        --     EELAYER END

                        -- This data goes into a temporarily sheet header (sheet_header_scratch). When the schematic file has been
                        -- read completely, the temporarily sheet header is appended to a list of headers. 
                        -- Why a list of headers ? When schematic files are exported, their headers must be restored to the original state.

                        -- used libraries from lines like "LIBS:bel_stm32" , CS: not used ?
                        if get_field(text_in => to_string(line_of_schematic_file), position => 1, ifs => latin_1.colon) = schematic_library then

                            -- CS: if this entry is without meaning, it should not go into the report at all
                            put_line(" uses library " & get_field(text_in => to_string(line_of_schematic_file), position => 2, ifs => latin_1.colon));

                            -- append library to list of libraries of the temporarily sheet header
                            type_list_of_library_names.append(
                                container => sheet_header_scratch.libraries,
                                new_item => type_library_name.to_bounded_string(get_field(text_in => to_string(line_of_schematic_file), position => 2, ifs => latin_1.colon))
                                );
                        end if;

                        -- layer numbers from a line like "EELAYER 25 0" -- CS: not used ?
                        -- CS: we do not read the line "EELAYER END" and assume it is always there.                                                        
                        if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_eelayer then
                            if get_field(text_in => to_string(line_of_schematic_file), position => 2) = schematic_eelayer_end then
                                null;
                            else
                                -- append layer numbers to the temporarily sheet header
                                sheet_header_scratch.eelayer_a := positive'value(
                                    get_field(text_in => to_string(line_of_schematic_file), position => 2));

                                sheet_header_scratch.eelayer_b := natural'value(
                                    get_field(text_in => to_string(line_of_schematic_file), position => 3));
                            end if;
                        end if;
                        
                        
                        
                        -- READ DESCRIPTION:
                        -- If the description reveals there is more than one sheet, we have a hierarchic design. Means we
                        -- need to read follwing sheet sections.
                        -- The sheet_number_current obtained here serves as part of the coordinates of objects found on this sheet.
                        -- The sheet description looks like this:

                        -- $Descr A4 11693 8268
                        -- encoding utf-8
                        -- Sheet 5 8
                        -- Title ""
                        -- Date ""
                        -- Rev ""
                        -- Comp ""
                        -- Comment1 ""
                        -- Comment2 ""
                        -- Comment3 ""
                        -- Comment4 ""
                        -- $EndDescr
                        
                        if not description_entered then
                            if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_description_header then -- $Descr A4 11693 8268
                                description_entered := true; -- we are entering the sheet description

                                -- read drawing frame dimensions from a line like "$Descr A4 11693 8268"
                                drawing_frame_scratch.paper_size := type_paper_size'value(get_field(text_in => to_string(line_of_schematic_file), position => 2));
                                drawing_frame_scratch.size_x := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 3));
                                drawing_frame_scratch.size_y := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 4)); 
                                drawing_frame_scratch.coordinates.path := path_to_submodule;
                                drawing_frame_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));

                                -- CS: Other properties of the drawing frame like x/y coordinates, lists of lines and texts are 
                                -- kicad built-in things and remain unassigned here.
                                                            
                            end if;
                        else -- we are inside the description
                            if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_description_footer then -- $EndDescr
                                description_entered := false; -- we are leaving the description
                                description_processed := true;

                                -- Make temporarily title_block_scratch complete by assigning coordinates and list of texts.
                                -- Then purge temporarily list of texts.
                                -- Then append temporarily title block to main module.
                                title_block_scratch.coordinates.path := path_to_submodule;
                                title_block_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));                            
                                title_block_scratch.texts := list_of_title_block_texts_scratch; -- assign collected texts list to temporarily title block
                                -- CS: x/y coordinates and list of lines are kicad built-in things and thus not available currently.

                                -- purge temporarily texts
                                type_list_of_title_block_texts.delete(list_of_title_block_texts_scratch,1,
                                    type_list_of_title_block_texts.length(list_of_title_block_texts_scratch));

                                -- append title block to main module
                                type_list_of_title_blocks.append(module.title_blocks,title_block_scratch);
                                
                                -- append temporarily drawing frame to main module
                                type_list_of_frames.append(module.frames,drawing_frame_scratch);
                            end if;

                            -- read endcoding from a line like "encoding utf-8"
                            -- CS: checks only for a non-default endcoding and outputs a warning.
                            -- CS: we assume only one encoding. other encodings are ignored currently.
                            -- The encoding should be project wide. KiCad allows a sheet specific encoding which is no
                            -- good idea.
                            if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_encoding then
                                if get_field(text_in => to_string(line_of_schematic_file), position => 2) /= encoding_default then
                                    put_line(message_warning & "non-default endcoding '" & 
										get_field(text_in => to_string(line_of_schematic_file), position => 2) & "' found !");
                                end if;
                            end if;
                                
                            -- read sheet number from a line like "Sheet 1 7"
                            if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_sheet then
                                sheet_number_current := positive'value(get_field(text_in => to_string(line_of_schematic_file), position => 2));
                                put_line(" sheet" & positive'image(sheet_number_current) & " ...");
                                sheet_count_total    := positive'value(get_field(text_in => to_string(line_of_schematic_file), position => 3));
                                if sheet_count_total > 1 then
                                    -- Set in the list_of_submodules (to be returned) the parent_module. The schematic file 
                                    -- being processed (see input parameters of read_file_schematic_kicad) becomes the parent module
                                    -- of the submodules here.
                                    list_of_submodules.parent_module := type_submodule_name.to_bounded_string(to_string(name_of_schematic_file));
                                end if;
                                -- CS: make sure total sheet count is less or equal current sheet number.

                                -- Our temporarily drawing frame gets the current sheet number assigned.
                                drawing_frame_scratch.coordinates.sheet_number := sheet_number_current;
                            end if;						

                            -- read sheet title from a line like "Title "abc""
                            if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_title then                        
                                title_block_text_scratch.meaning := TITLE;
                                title_block_text_scratch.text := type_device_block_text_string.to_bounded_string(
                                    strip_quotes((get_field(text_in => to_string(line_of_schematic_file), position => 2))));
                                type_list_of_title_block_texts.append(list_of_title_block_texts_scratch,title_block_text_scratch);
                            end if;

                            -- read date from a line like "Date "1981-01-23""
                            if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_date then                        
                                title_block_text_scratch.meaning := DRAWN_DATE;
                                title_block_text_scratch.text := type_device_block_text_string.to_bounded_string(
                                    strip_quotes((get_field(text_in => to_string(line_of_schematic_file), position => 2))));
                                type_list_of_title_block_texts.append(list_of_title_block_texts_scratch,title_block_text_scratch);
                            end if;

                            -- read revision from a line like "Rev "9.7.1"
                            if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_revision then                        
                                title_block_text_scratch.meaning := REVISION;
                                title_block_text_scratch.text := type_device_block_text_string.to_bounded_string(
                                    strip_quotes((get_field(text_in => to_string(line_of_schematic_file), position => 2))));
                                type_list_of_title_block_texts.append(list_of_title_block_texts_scratch,title_block_text_scratch);
                            end if;

                            -- read company name
                            if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_company then
                                title_block_text_scratch.meaning := COMPANY;
                                title_block_text_scratch.text := type_device_block_text_string.to_bounded_string(
                                    strip_quotes((get_field(text_in => to_string(line_of_schematic_file), position => 2))));
                                type_list_of_title_block_texts.append(list_of_title_block_texts_scratch,title_block_text_scratch);
                            end if;

                            -- read commments 1..4 CS: need something more flexible here in order to read any number of comments.
                            if  get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_comment_1 or
                                get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_comment_2 or
                                get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_comment_3 or 
                                get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_comment_4 then
                                    title_block_text_scratch.meaning := MISC;
                                    title_block_text_scratch.text := type_device_block_text_string.to_bounded_string(
                                        strip_quotes((get_field(text_in => to_string(line_of_schematic_file), position => 2))));
                                    type_list_of_title_block_texts.append(list_of_title_block_texts_scratch,title_block_text_scratch);
                            end if;
                            

                        end if;

                        -- Read submodule (sheet) sections (if there has been a total sheet count greater 1 detected earlier).
                        -- NOTE: Such sections solely serve to display a hierarchical sheet as a black box with its ports.
                        -- Rightly said this is the black box representation of a submodule. 
                        -- So in the following we refer to them as "submodule".
                        -- A submodule (sheet) section example:
                        
                        -- $Sheet
                        -- S 4050 5750 1050 650 
                        -- U 58A73B5D
                        -- F0 "Sheet58A73B5C" 58
                        -- F1 "morpho_test.sch" 58
                        -- $EndSheet

                        -- And add name of submodule (sheet file name) to list_of_submodules:
                        if sheet_count_total > 1 then
                            if not sheet_description_entered then
                                if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_sheet_header then -- $Sheet
                                    sheet_description_entered := true;
                                end if;
                            else -- we are inside a sheet description
                                if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_sheet_footer then -- $EndSheet
                                    sheet_description_entered := false; -- we are leaving the sheet description

                                    -- append name_of_submodule_scratch to list_of_submodules to be returned to parent unit
                                    type_list_of_submodule_names.append(list_of_submodules.list, name_of_submodule_scratch);

                                    -- append submodule_gui_scratch to list of gui submodules
                                    type_list_of_gui_submodules.append(module.submodules, submodule_gui_scratch);
                                end if;

                                -- read GUI submodule (sheet) position and size from a line like "S 4050 5750 1050 650"
                                if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_sheet_pos_and_size then
                                    submodule_gui_scratch.coordinates.path := path_to_submodule;
                                    submodule_gui_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                                    submodule_gui_scratch.coordinates.sheet_number := sheet_number_current;
                                    submodule_gui_scratch.coordinates.x := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 2));
                                    submodule_gui_scratch.coordinates.y := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 3));
                                    submodule_gui_scratch.size_x := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 4));
                                    submodule_gui_scratch.size_y := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 5));                                
                                end if;

                                -- read GUI submodule (sheet) timestamp from a line like "U 58A73B5D"
                                if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_sheet_timestamp then 
                                    submodule_gui_scratch.timestamp := get_field(text_in => to_string(line_of_schematic_file), position => 2);
                                end if;
                                
                                -- Read submodule (sheet) name from a line like "F0 "mcu_stm32f030" 60"
                                -- Since this is the black-box-representation of a kicad-sheet its name is threated as name of a submodule.
                                -- The sheet name is stored in submodule_gui_scratch.name to be compared with the sheet file name later.
                                if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_sheet_name then
                                    submodule_gui_scratch.name := type_submodule_name.to_bounded_string(strip_quotes(get_field(text_in => to_string(line_of_schematic_file), position => 2)));
                                    submodule_gui_scratch.text_size_of_name := type_text_size'value(get_field(text_in => to_string(line_of_schematic_file), position => 3));
                                end if;

                                -- Read sheet file name from a line like "F1 "mcu_stm32f030.sch" 60".
                                -- The file name (name_of_submodule_scratch) goes into the list of submodules to be returned to the parent unit.
                                if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_sheet_file then
                                    name_of_submodule_scratch := type_submodule_name.to_bounded_string(strip_quotes(get_field(text_in => to_string(line_of_schematic_file), position => 2)));
                                    submodule_gui_scratch.text_size_of_file := type_text_size'value(get_field(text_in => to_string(line_of_schematic_file), position => 3));
                                    
                                    -- Test if sheet name and file name match:
                                    if type_submodule_name.to_string(submodule_gui_scratch.name) /= base_name(type_submodule_name.to_string(name_of_submodule_scratch)) then
                                        put_line(message_warning & "name mismatch: sheet: " &
											type_submodule_name.to_string(submodule_gui_scratch.name) &
											" file: " & type_submodule_name.to_string(name_of_submodule_scratch));
                                    end if;
                                end if;

                            end if;
                        end if;

                        -- Further parts of the file can be read IF the description has been processed before (see above)
                        if description_processed then
                            
                            -- read net segments						
                            if not net_segment_entered then
                                -- collect net segments
                                if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_wire then
                                    if get_field(text_in => to_string(line_of_schematic_file), position => 2) = schematic_keyword_wire then
                                        if get_field(text_in => to_string(line_of_schematic_file), position => 3) = schematic_keyword_line then
                                            --put_line(to_string(line_of_schematic_file));
                                            net_segment_entered := true; -- CS: assumption: segment coordinates follow in next line
                                        end if;
                                    end if;
                                end if;
                            else
                                net_segment_entered := false; -- we are leaving a net segment
                                --put_line(to_string(line_of_schematic_file));

                                -- CS: warning on segment with zero length
                                
                                -- Build a temporarily net segment with fully specified coordinates:
                                segment_scratch.coordinates_start.path := path_to_submodule;
                                
                                -- the name of the current submodule, which is in case of kicad the subordinated schematic file
                                segment_scratch.coordinates_start.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                                segment_scratch.coordinates_end.module_name   := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                                
                                -- The sheet number. NOTE: Kicad V4 can handle only one sheet per submodule. The sheet numbering is consecutive and does
                                -- not care about the actual submodule names.
                                segment_scratch.coordinates_start.sheet_number := sheet_number_current;

                                -- the x/y position
                                segment_scratch.coordinates_start.x := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 1));
                                segment_scratch.coordinates_start.y := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 2));
                                segment_scratch.coordinates_end.x   := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 3));
                                segment_scratch.coordinates_end.y   := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 4));

                                -- The net segments are to be collected in a wild list of segments for later sorting. 
                                type_wild_list_of_net_segments.append(wild_segment_collection,segment_scratch);
                            end if;

                            -- read net junctions and store them in a wild list of net junctions for later sorting
                            if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_connection then
                                if get_field(text_in => to_string(line_of_schematic_file), position => 2) = schematic_tilde then

                                    -- build a temporarily junction
                                    junction_scratch.coordinates.path := path_to_submodule;
                                    junction_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                                    junction_scratch.coordinates.sheet_number := sheet_number_current;
                                    junction_scratch.coordinates.x := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 3));
                                    junction_scratch.coordinates.y := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 4));
                                    type_list_of_net_junctions.append(wild_collection_of_junctions,junction_scratch);
                                    junction_count := junction_count + 1;
                                end if;
                            end if;
                                
                            -- Read simple net labels (they do not have a tag, but just a text) 
                            -- CS: assumption: keywords "Text Label" and coordinates in one line
                            if not simple_label_entered then							
                                if 	get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_text and 
                                    get_field(text_in => to_string(line_of_schematic_file), position => 2) = schematic_keyword_label_simple then

                                    simple_label_entered := true;
                                    --put_line(to_string(line_of_schematic_file));

                                    -- Build a temporarily simple label from a line like "Text Label 5350 3050 0    60   ~ 0" :
                                    label_simple_scratch.coordinates.path := path_to_submodule;
                                    label_simple_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                                    label_simple_scratch.coordinates.sheet_number := sheet_number_current;
                                    label_simple_scratch.coordinates.x := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 3));
                                    label_simple_scratch.coordinates.y := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 4));
                                    label_simple_scratch.orientation   := to_orientation(get_field(text_in => to_string(line_of_schematic_file), position => 5));

                                    -- build text attributes from size, font and line width
                                    label_simple_scratch.text_attributes := to_text_attributes(
                                        size  => type_text_size'value(get_field(text_in => to_string(line_of_schematic_file), position => 6)),
                                        style => get_field(text_in => to_string(line_of_schematic_file), position => 7),
                                        width => type_text_line_width'value(get_field(text_in => to_string(line_of_schematic_file), position => 8)));
                                end if;
                            else
                                simple_label_entered := false; -- we are leaving a simple label

                                -- get label text and put it to temporarily simple label
                                label_simple_scratch.text := type_net_name.to_bounded_string(get_field(text_in => to_string(line_of_schematic_file), position => 1));

                                -- The simple labels are to be collected in a wild list of simple labels.
                                write_coordinates_of_label( type_net_label(label_simple_scratch));
                                type_list_of_labels_simple.append(wild_simple_label_collection_scratch,label_simple_scratch);
                            end if;
                            
                            -- read tag net labels (tagged labels can be global or hierarchical)
                            if not tag_label_entered then
                                if 	get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_text 
                                    and 
                                    (get_field(text_in => to_string(line_of_schematic_file), position => 2) = schematic_keyword_label_hierarchic 
                                    or get_field(text_in => to_string(line_of_schematic_file), position => 2) = schematic_keyword_label_global)
                                    then
                                
                                    tag_label_entered := true;
                                    --put_line(to_string(line_of_schematic_file));

                                    -- Build a temporarily hierarchic/global label from a line like "Text GLabel 1850 3100 0 58 BiDi ~ 0"
                                    -- The keyword in field 2 tells whether we have a hierarchic or global label:
                                    if get_field(text_in => to_string(line_of_schematic_file), position => 2) = schematic_keyword_label_hierarchic then
                                        label_tag_scratch.hierarchic := true;
                                        label_tag_scratch.global := false;
                                    else
                                        label_tag_scratch.hierarchic := false;
                                        label_tag_scratch.global := true;
                                    end if;

                                    label_tag_scratch.coordinates.path := path_to_submodule;
                                    label_tag_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                                    label_tag_scratch.coordinates.sheet_number := sheet_number_current;
                                    label_tag_scratch.coordinates.x := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 3));
                                    label_tag_scratch.coordinates.y := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 4));
                                    label_tag_scratch.orientation   := to_orientation(get_field(text_in => to_string(line_of_schematic_file), position => 5));
                                    
                                    label_tag_scratch.direction := to_direction(
                                        get_field(text_in => to_string(line_of_schematic_file), position => 7)
                                        );

                                    -- build text attributes from size, font and line width
                                    label_tag_scratch.text_attributes := to_text_attributes(
                                        size  => type_text_size'value(get_field(text_in => to_string(line_of_schematic_file), position => 6)),
                                        style => get_field(text_in => to_string(line_of_schematic_file), position => 8),
                                        width => type_text_line_width'value(get_field(text_in => to_string(line_of_schematic_file), position => 9)));
                                end if;
                            else
                                tag_label_entered := false; -- we are leaving a tag label

                                -- get label text and put it to temporarily tag label
                                label_tag_scratch.text := type_net_name.to_bounded_string(get_field(text_in => to_string(line_of_schematic_file), position => 1));

                                -- The tag labels are to be collected in a wild list of tag labels for later sorting.
                                type_list_of_labels_tag.append(wild_tag_label_collection_scratch,label_tag_scratch);
                            end if;

                            -- read note from a line like "Text Notes 3400 2800 0 60 Italic 12" followed by a line with the actual note:
                            if not note_entered then
                                if 	get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_keyword_text and 
                                    get_field(text_in => to_string(line_of_schematic_file), position => 2) = schematic_keyword_note then
                                        note_entered := true; -- we are entering a note

                                        -- set coordinates
                                        note_scratch.coordinates.path := path_to_submodule;
                                        note_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                                        note_scratch.coordinates.sheet_number := sheet_number_current;
                                        note_scratch.coordinates.x := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 3));
                                        note_scratch.coordinates.y := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 4));
                                        note_scratch.orientation   := to_orientation(get_field(text_in => to_string(line_of_schematic_file), position => 5));

                                        -- build text attributes from size, font and line width
                                        note_scratch.text_attributes := to_text_attributes(
                                            size  => type_text_size'value(get_field(text_in => to_string(line_of_schematic_file), position => 6)),
                                            style => get_field(text_in => to_string(line_of_schematic_file), position => 7),
                                            width => type_text_line_width'value(get_field(text_in => to_string(line_of_schematic_file), position => 8)));
                                end if;
                            else 
                                note_entered := false; -- we are leaving a note

                                -- get note text from a line like "hello\ntest". NOTE "\n" represents a line break
                                -- CS: store lines in a list of lines instead ?
                                note_scratch.text := to_unbounded_string(get_field(text_in => to_string(line_of_schematic_file), position => 1));

                                -- the notes are to be collected in the list of notes
                                type_list_of_notes.append(module.notes,note_scratch);
                            end if;
                            
                            -- READ COMPONENTS
                            -- Once a component header ($Comp) found, set device_entered flag. This indicates we are inside a device section.
                            -- Inside the device section, we process its content until the component footer is found.
                            if not device_entered then
                                if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_component_header then
                                    device_entered := true;
                                end if;
                            else -- we are inside the component
                                if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_component_footer then
                                    device_entered := false; -- we are leaving the component

                                    --put_line(to_string(line_of_schematic_file));								
                                    -- update the device with the collected block data (in device_block_scratch)
                                    type_device_list_of_module.update_element(module.devices,device_cursor_scratch, append_block'access);

                                    -- clean up: the list of texts collected in device_block_scratch.text_list must be erased for next spin.
                                    type_list_of_device_block_texts.delete(device_block_scratch.text_list,1,type_list_of_device_block_texts.length(device_block_scratch.text_list));
                                else
                                    -- put_line(to_string(line_of_schematic_file));
                                    -- READ COMPONENT SECTION CONTENT
                                    
                                    -- Read device name and annotation from a line like "L NetChanger N1". 
                                    -- Append the device to the device list of the module. Devices may occur multiple times, which implies they are
                                    -- split into blocks (kicad refers to them as "units", EAGLE refers to them as "symbols").
                                    -- Only the first occurence of the device leads to appending it to the device list of the module.
                                    if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_component_identifier_name then -- "L"
                                        device_scratch.name_in_library := type_device_name_in_library.to_bounded_string(
                                            get_field(text_in => to_string(line_of_schematic_file), position => 2)); -- "NetChanger"
                                        device_scratch.annotation := type_device_name.to_bounded_string(
                                            get_field(text_in => to_string(line_of_schematic_file), position => 3)); -- "N1"
                                        -- CS: check annotation

                                        -- If component is not in device list yet, add component to device list of module.
                                        if not type_device_list_of_module.contains(module.devices,device_scratch) then
    -- 										put(et_import.report_handle, " device: " &
    -- 											type_device_name.to_string(device_scratch.annotation) & " is " &
    -- 											type_device_name_in_library.to_string(device_scratch.name_in_library));

                                            put("  device " & type_device_name.to_string(device_scratch.annotation) & " is " &
												type_device_name_in_library.to_string(device_scratch.name_in_library));
                                                    
                                            type_device_list_of_module.append(module.devices,device_scratch);
                                        end if;

                                        -- The cursor device_cursor_scratch now points to the device. There will be more device information (in the following) 
                                        -- that will go into device_scratch. Once the device section is left, device_scratch updates the device where the cursor
                                        -- is pointing at.
                                        device_cursor_scratch := type_device_list_of_module.find(module.devices,device_scratch);
                                    end if;

                                    -- read unit id from a line like "U 7 3 4543D4D3F"
                                    if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_component_identifier_unit then -- "U"
                                        --put_line(to_string(line_of_schematic_file));

                                        -- KiCad uses positive numbers to identifiy blocks (units). In general a block name can be a string as well.
                                        -- Therefore we handle the block id as string.
                                        -- Temporarily the block data is collected in device_block_scratch (to update the device later when leaving the device section).
                                        -- We also verify here that the block id is not greater than the total number of blocks (in field 2).
                                        if 	positive'value(get_field(text_in => to_string(line_of_schematic_file), position => 3)) > -- "3" -- id
                                            positive'value(get_field(text_in => to_string(line_of_schematic_file), position => 2))   -- "7" -- total
                                            then
												new_line;
												put_line(message_warning & "Unit ID greater than number of units !");
                                        end if;
                                        device_block_scratch.name := type_device_block_name.to_bounded_string(
                                            get_field(text_in => to_string(line_of_schematic_file), position => 3)); -- "3"

                                        --put(et_import.report_handle," with block " & type_device_block_name.to_string(device_block_scratch.name) & " at");

                                        put(" with block " & type_device_block_name.to_string(device_block_scratch.name) & " at");
                                        
                                    end if;

                                    -- Read unit/block coordinates from a line like "P 3200 4500".
                                    -- The unit/block coordinates is more than just x/y !
                                    -- The wirte the unit coordinates in the import report.
                                    if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_component_identifier_coord then -- "P"
                                        device_block_scratch.coordinates.x := type_grid'value(
                                            get_field(text_in => to_string(line_of_schematic_file), position => 2)); -- "3200"
                                        device_block_scratch.coordinates.y := type_grid'value(
                                            get_field(text_in => to_string(line_of_schematic_file), position => 3)); -- "4500"

    --									device_block_scratch.coordinates.main_module := module.name;
                                        device_block_scratch.coordinates.path := path_to_submodule;
                                        device_block_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                                        device_block_scratch.coordinates.sheet_number := sheet_number_current;

                                        write_coordinates_of_device_block(device_block_scratch);
                                    end if;

                                    -- read component fields 0..2 from lines like:
                                    -- 			"F 0 "N701" H 2600 2100 39  0000 C CNN"
                                    --			"F 1 "NetChanger" H 2600 2250 60  0001 C CNN"
                                    --			"F 2 "bel_netchanger:N_0.2MM" H 2600 2100 60  0001 C CNN"
                                    -- Each line represents a text field which goes temporarily into block_text_scratch. 
                                    -- Once the line is processed, block_text_scratch is appended the the list of texts of device_block_scratch.
                                    if get_field(text_in => to_string(line_of_schematic_file), position => 1) = schematic_component_identifier_field then -- "F"

                                        -- The field id must be mapped to the actual field meaning:
                                        case type_schematic_component_field_id'value(get_field(text_in => to_string(line_of_schematic_file), position => 2)) is -- "0..2"
                                            when schematic_component_field_id_annotation => block_text_scratch.meaning := annotation; -- "0"
                                            when schematic_component_field_id_value => block_text_scratch.meaning := value; -- "1"
                                            when schematic_component_field_id_footprint => block_text_scratch.meaning := footprint; -- "2"
                                            --CS: when schematic_component_field_id_partcode => block_text_scratch.meaning := partcode;
                                            when others => block_text_scratch.meaning := misc;
                                        end case;
                                        
                                        -- read content like "N701" or "NetChanger" from field position 3
                                        block_text_scratch.text := type_device_block_text_string.to_bounded_string(strip_quotes( 
                                            get_field(text_in => to_string(line_of_schematic_file), position => 3)));

                                        -- read orientation like "H" -- type_schematic_field_orientation
                                        case type_schematic_field_orientation'value(get_field(text_in => to_string(line_of_schematic_file), position => 4)) is
                                            when H => block_text_scratch.orientation := deg_0;
                                            when V => block_text_scratch.orientation := deg_90;
                                        end case;

                                        -- read x and y coordinates like 2600 3250
                                        block_text_scratch.coordinates.x := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 5));
                                        block_text_scratch.coordinates.y := type_grid'value(get_field(text_in => to_string(line_of_schematic_file), position => 6));

                                        -- assign further coordinates
                                        block_text_scratch.coordinates.path := path_to_submodule;
                                        block_text_scratch.coordinates.module_name := type_submodule_name.to_bounded_string( to_string(name_of_schematic_file));
                                        block_text_scratch.coordinates.sheet_number := sheet_number_current;

                                        -- build text attributes (only text size available here, style and width assume default value)
                                        block_text_scratch.text_attributes := to_text_attributes(
                                            size => type_text_size'value(get_field(text_in => to_string(line_of_schematic_file), position => 7)),
                                            style => schematic_style_normal,
                                            width => 0);
                                            
                                        -- build text alignment
                                        block_text_scratch.visible := to_visible(get_field(text_in => to_string(line_of_schematic_file), position => 8));
                                        block_text_scratch.alignment_horizontal := to_alignment_horizontal(get_field(text_in => to_string(line_of_schematic_file), position => 9));
                                        block_text_scratch.alignment_vertical := to_alignment_vertical(get_field(text_in => to_string(line_of_schematic_file), position => 10));  
                                        
                                        -- append text block_text_scratch to text list of scratch block.
                                        type_list_of_device_block_texts.append(device_block_scratch.text_list,block_text_scratch);

                                    end if;
                                end if;
                                
                            end if;
                        end if;

                    end if; -- if not schematic_header_processed
                end loop;

                -- If file has been read and no header found:
                if not schematic_headline_processed then
                    write_message(
						file_handle => current_output,
						text => message_error & "Schematic file header invalid or not found ! File not accepted !",
						console => true);
                    raise constraint_error;
                end if;

                -- Add temporarily sheet_header_scratch to list of headers.
                -- NOTE: The file name serves as key in order to match file with header.
                type_list_of_sheet_headers.insert(
                    container => list_of_sheet_headers, 
                    key => type_sheet_file.to_bounded_string(to_string(name_of_schematic_file)),
                    new_item => sheet_header_scratch);
                
				close ( et_import.schematic_handle );
				
				-- Build anonymous nets:
				-- We are processing the net segments of a sheet here. The net segments have been collected in a wild list of net segments earlier.
				-- This list does not reveal the actual nets where the segments belong to. The segments are inspected in the following by
				-- looking at the coordinates of their start and end point. Segments whose start or end points match other segments are considered
				-- as connected to each other (means they belong to the same net). The net name is unknown yet. So the outcome of the following is
				-- a list of anonymous nets.
				-- CS: handle circlular nets, currently they cause a forever-loop here
				segment_count := natural(type_wild_list_of_net_segments.length(wild_segment_collection)); -- get number of segments on the current sheet
				put_line("  processing" & natural'image(segment_count) & " net segments ...");

				-- It may happen that a sheet has no nets, for example the top level sheet of a design with global nets only. If there are no net segments
				-- at all, skip processing net segments.
				if segment_count > 0 then 

					-- Segments where a junction sits on, must be broken down. This results in more segments than calculated earlier.
					-- The outcome of process_junctions might be a greater number of net segments than currently being held in segment_count.
					process_junctions;
					-- segment_count now has been updated
					
					-- We inspect one segment after an other. Variable seg points to the segment being processed. 
					-- A segment, whose e AND s flag has been set, is to be skipped (because this segment has been processed already).
					-- Variable side_scratch points to the side of the segment (start or end point) where another matching segment is to be searched for.
					-- If a matching segment is found, it gets appended to the current anonymous net.
					for i in 1..segment_count loop
						seg := i; 
						if not type_wild_list_of_net_segments.element(wild_segment_collection,seg).s and -- Skip already processed nets.
						   not type_wild_list_of_net_segments.element(wild_segment_collection,seg).e then 

						    -- We initiate a new anonymous net and start looking for a matching segment on the end_point:
							--put_line(et_import.report_handle," anonymous net" & positive'image(seg) & ":"); 
							put_line("  anonymous net with segments:");
											
							add_segment_to_anonymous_net(seg); 
							side_scratch := end_point;

							loop -- A
								--put_line(et_import.report_handle,"  --> A");
								-- Search for a segment connected to the current segment. 
								-- If function search_for_same_coordinates discovers a suitable segment, it adds the segment to current anonymous net.
								-- search_for_same_coordinates sets the e or s flag of the segment in order to indicate which end point has been processed.
								-- If no connected segment found, toggle side_scratch and repeat search_for_same_coordinates on the other side of the segment.
								same_coord_result := search_for_same_coordinates( id => seg, seg_in => type_wild_list_of_net_segments.element(wild_segment_collection,seg),
																				  side => side_scratch);
								if same_coord_result.valid then
									--put_line(et_import.report_handle,"  --> E");
									null;
								else
									-- Toggle side_scratch depending on the e/s flag of the segment:
									-- D
 									if type_wild_list_of_net_segments.element(wild_segment_collection,seg).e then
										-- put_line(et_import.report_handle,"  --> D1");
										side_scratch := start_point;
									end if;
									
 									if type_wild_list_of_net_segments.element(wild_segment_collection,seg).s then
										-- put_line(et_import.report_handle,"  --> D2");	
 										side_scratch := end_point;	
 									end if;

									-- C
									--put_line(et_import.report_handle,"  --> C");
									-- Search for a segment connected to the current segment (starting from the current side_scratch).
									-- If function search_for_same_coordinates discovers a suitable segment, it adds the segment to current anonymous net.
									-- search_for_same_coordinates sets the e or s flag of the segment in order to indicate which end point has been processed.
									-- If no connected segment found, the current anonymous net is considered as complete -> cancel loop, advance to next segment ...
									same_coord_result := search_for_same_coordinates( id => seg, seg_in => type_wild_list_of_net_segments.element(wild_segment_collection,seg),
																					  side => side_scratch);
									if same_coord_result.valid then
										--put_line(et_import.report_handle,"  --> F");	
										null;
									else
										--put_line(et_import.report_handle,"  done");
										add_net_to_list_of_anonymous_nets; 	-- All collected segments belong to the same net. This net is to be added to
																			-- a list of anonymous nets.
										exit;	-- no further segment search required.
									end if;
								end if;

								-- B
								--put_line(et_import.report_handle,"  --> B");
								-- Update seg with the id of the segment found by search_for_same_coordinates. So seg now points to the next connected segment. 
								-- same_coord_result contains the end point of the net that has been found.
								-- Depending on the end point of the matching net, side_scratch must be set so that the search can continue on the other side
								-- of the new segment.
								seg := same_coord_result.id;
								case same_coord_result.side is
									when end_point => 
										side_scratch := start_point;
									when start_point =>
										side_scratch := end_point;
								end case;
							end loop;
						end if;
					end loop;
				end if;


				-- All anonymous nets must be given a name. The name is enforced by the a net label. The first label found on the net sets the net name.
				-- Other labels on the net are checke for their name only. If the name differs from the net name set earlier, a warning is output.
				-- Nets without label remain anonymous by using the notation "N$"
				associate_net_labels_with_anonymous_nets;

				-- The components collected from the schematic file lack the pin positions. We need the pin positions in order to generate
				-- a netlist.
				fetch_components_from_library;
				
			else
				--put_line(message_error & "Schematic file '" & to_string(name_of_schematic_file) & "' not found !");
				write_message(
					file_handle => current_output,
					text => message_error & "schematic file '" & to_string(name_of_schematic_file) & "' not found !",
					console => true);
				raise constraint_error;
			end if;

			return list_of_submodules;
			
			exception
				when event:
					constraint_error =>
						put_line(exception_information(event));
						put_line(message_error & "in schematic file '" & to_string(name_of_schematic_file) & "' line" & natural'image(line_counter));
						raise;
						return list_of_submodules;
				when others =>
						put_line(message_error & "in schematic file '" & to_string(name_of_schematic_file) & "' line" & natural'image(line_counter));
						raise;					
						return list_of_submodules;

		end read_schematic;


		list_of_submodules : type_list_of_submodule_names_extended;
		top_level_schematic_file, name_of_schematic_file : et_import.type_schematic_file_name.bounded_string;

		package stack_of_sheet_lists is new stack_lifo(max => 10, item => type_list_of_submodule_names_extended);
        use stack_of_sheet_lists;
        
    begin -- import design
		create_report;
		
		case et_import.cad_format is
			when kicad_v4 =>

				-- derive top level schematic file name from project file (they differ only in their extension)
				top_level_schematic_file := read_project_file;
				name_of_schematic_file := top_level_schematic_file;

                -- The top level schematic file dictates the module name. At the same time it is the first entry
                -- in the module path.
				module.name := type_submodule_name.to_bounded_string(to_string(name_of_schematic_file));
                append_name_of_parent_module_to_path(module.name);
                
				-- Starting from the top level module, we read its schematic file. The result can be a list of submodules.
				-- NOTE: Kicad refers to them as "sheets" !
				
				-- The function read_schematic requires the name of the current submodule,
				list_of_submodules := read_schematic(name_of_schematic_file => name_of_schematic_file);
				
				put("  DESIGN STRUCTURE ");

				-- If read_file_schematic_kicad returns an empty list of submodules, we are dealing with a flat design. Otherwise
				-- the design is hierarchic (because the submodule list is longer than zero).
				if type_list_of_submodule_names.length(list_of_submodules.list) = 0 then -- flat design
					put_line("FLAT");
				else -- hierarchic design
					-- In the follwing we dive into hierarchic levels. Each time before a deeper level is entered,
					-- the list of submodules of the current level is saved on a LIFO stack.
					-- The top level schematic is at level 0. The level decreases each time a deeper level is assumed.
					put_line("HIERARCHIC");
					stack_of_sheet_lists.init; -- stack init

					-- output the number of submodules (sheets) found at level 0:
					put_line("  number of hierarchic sheets " & natural'image(
						natural(type_list_of_submodule_names.length(list_of_submodules.list))));

					-- Initially set submodule pointer at first submodule of list:
					list_of_submodules.id := 1;
                    
					loop
						-- fetch name of submodule (where id is pointing at)
						name_of_schematic_file := to_bounded_string(type_submodule_name.to_string(
							type_list_of_submodule_names.element(container => list_of_submodules.list,index => list_of_submodules.id)));
						
						-- backup list_of_submodules OF THIS LEVEL on stack (including the current submodule id)
						push(list_of_submodules);
						put_line("DESCENDING TO HIERARCHY LEVEL -" & trim(natural'image(depth),left));
						put_line(row_separator_single);
						
						-- Read schematic file as indicated by list_of_submodules.id. 
						-- Read_schematic receives the name of the schematic file to be read.
						list_of_submodules := read_schematic(name_of_schematic_file => name_of_schematic_file);

						-- If the schematic file contains submodules (hierarchic sheets), set list_of_submodules.id to the first 
						-- submodule of them. Otherwise restore submodule list of parent module and advance therein to next submodule.
						if type_list_of_submodule_names.length(list_of_submodules.list) = 0 then -- flat submodule (no hierarchic sheets)

							list_of_submodules := pop;
                            list_of_submodules.id := list_of_submodules.id + 1;
                            --delete_last_module_name_from_path;
							put_line("NO SUBMODULES HERE. ASCENDING TO HIERARCHY LEVEL -" & trim(natural'image(depth),left));
							put_line(row_separator_single);

						else
							-- set cursor at first submodule of list and append name of parent module to path_to_submodule
                            list_of_submodules.id := 1;
                            append_name_of_parent_module_to_path(list_of_submodules.parent_module);
						end if;

						-- Once the last submodule of the list has been processed, restore list of the overlying level and advance to next module.
						-- Exit after last submodule in level 0 has been processed.
						if list_of_submodules.id > positive(type_list_of_submodule_names.length(list_of_submodules.list)) then
							if depth = 0 then 
								put_line("LAST SUBMODULE PROCESSED.");
								exit; 
							end if;
							list_of_submodules := pop; -- restore overlying list
                            list_of_submodules.id := list_of_submodules.id + 1;
                            delete_last_module_name_from_path; -- update path_to_submodule
							put_line("LAST SUBMODULE PROCESSED. ASCENDING TO HIERARCHY LEVEL: -" & trim(natural'image(depth),left));
							put_line(row_separator_single);
						end if;
						
					end loop;
				end if;
			when others =>
				null;
		end case;

		close_report;

		-- CS: exception handler
		
	end import_design;

end et_kicad;

