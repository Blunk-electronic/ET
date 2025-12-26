------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             PACKAGE READ                                 --
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

with ada.characters.handling;	use ada.characters.handling;
with ada.strings; 				use ada.strings;
with ada.text_io;				use ada.text_io;
with ada.exceptions;

with et_directory_and_file_ops;
with et_general_rw;						use et_general_rw;

with et_text;							use et_text;
with et_text_content;					use et_text_content;

with et_alignment;						use et_alignment;
with et_terminals;						use et_terminals;

with et_primitive_objects;				use et_primitive_objects;
with et_conductor_text.packages;		use et_conductor_text.packages;
with et_time;							use et_time;
with et_mirroring;						use et_mirroring;
with et_coordinates_formatting;			use et_coordinates_formatting;
with et_system_info;
with et_package_description;			use et_package_description;
with et_keywords;						use et_keywords;
with et_section_headers;				use et_section_headers;
with et_board_read;						use et_board_read;
with et_conductor_segment;
with et_package_sections;				use et_package_sections;
with et_package_model;					use et_package_model;
with et_pcb_signal_layers;				use et_pcb_signal_layers;


package body et_package_read is

	use pac_texts_fab_with_content;
	
	use pac_conductor_texts;

	
	
	
	procedure read_package (
		file_name 		: in pac_package_model_file.bounded_string; -- libraries/packages/S_SO14.pac
		check_layers	: in et_pcb_stack.type_layer_check := (check => et_pcb_stack.NO);
		log_threshold	: in type_log_level) 
	is
		file_handle : ada.text_io.file_type;

		line : type_fields_of_line;

		-- This is the section stack of the package model. 
		-- Here we track the sections. On entering a section, its name is
		-- pushed onto the stack. When leaving a section the latest section name is popped.
		max_section_depth : constant positive := 7; -- incl. section init

		package stack is new et_general_rw.stack_lifo (
			item	=> type_package_section,
			max 	=> max_section_depth);


	-- VARIABLES FOR TEMPORARILY STORAGE AND ASSOCIATED HOUSEKEEPING SUBPROGRAMS:

		-- Once the appearance has been read, a new package will be created where this 
		-- pointer is pointing at:
		packge					: access type_package_model;
		pac_appearance			: type_bom_relevant := bom_relevant_default;

		-- The description and technology will be assigned once the complete
		-- model has been read. See main of this procedure.
		pac_description			: pac_package_description.bounded_string; 
		pac_technology			: type_assembly_technology := assembly_technology_default;

		--pac_text				: et_packages.type_text_with_content;
		--pac_text				: pac_text_fab.type_text_fab;
		pac_text				: type_text_fab_with_content;
		--content					: et_text.pac_text_content.bounded_string;
		pac_text_placeholder	: type_text_placeholder;
	
		terminal_position		: type_position := origin_zero_rotation;

		tht_stop_mask_status			: type_stop_mask_status := stop_mask_status_default;
		tht_stop_mask_shape_top			: type_stop_mask_shape := stop_mask_shape_default;
		tht_stop_mask_shape_bottom		: type_stop_mask_shape := stop_mask_shape_default;		
		tht_stop_mask_contours_top		: type_stop_mask_contours;
		tht_stop_mask_contours_bottom	: type_stop_mask_contours;		

		tht_width_inner_layers	: type_track_width := type_track_width'first;
		tht_hole				: type_terminal_tht_hole := terminal_tht_hole_default;
		tht_drill_size			: type_drill_size_tht := type_drill_size_tht'first;
		tht_millings			: type_contour;

		terminal_name			: pac_terminal_name.bounded_string;
		terminal_technology		: type_assembly_technology := assembly_technology_default;
		tht_pad_shape			: type_pad_outline_tht;		
		smt_pad_shape			: type_contour;

		smt_pad_face			: type_face := face_default;

		smt_stop_mask_status	: type_stop_mask_status := stop_mask_status_default;
		smt_stop_mask_shape		: type_stop_mask_shape := stop_mask_shape_default;
		smt_stop_mask_contours	: type_stop_mask_contours;		

		-- NOTE: Solder paste is applied to SMT pads only.
		smt_solder_paste_status	: type_solder_paste_status := solder_paste_status_default;
		smt_stencil_shape		: type_stencil_modification := stencil_modification_default;
		smt_stencil_contours	: type_stencil_contours;
		--smt_stencil_shrink		: type_stencil_shrink := stencil_shrink_default;
		smt_stencil_shrink		: type_distance_positive := stencil_shrink_default;

		
		procedure read_text is
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
				expect_field_count (line, 7);

				-- extract position of note starting at field 2
				pac_text.position := to_position (line, 2);

			elsif kw = keyword_size then -- size 1.000
				expect_field_count (line, 2);
				pac_text.size := to_distance (f (line, 2));

			elsif kw = keyword_linewidth then -- linewidth 0.1
				expect_field_count (line, 2);
				pac_text.line_width := to_distance (f (line, 2));

			elsif kw = keyword_alignment then -- alignment horizontal center vertical center
				expect_field_count (line, 5);

				-- extract alignment starting at field 2
				pac_text.alignment := to_alignment (line, 2);
				
			elsif kw = keyword_content then -- content "keep clear"
				expect_field_count (line, 2); -- actual content in quotes !
				pac_text.content := to_content (f (line, 2));
				
			else
				invalid_keyword (kw);
			end if;
		end read_text;

		
		
		procedure read_placeholder is
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_position then -- position x 91.44 y 118.56 rotation 45.0
				expect_field_count (line, 7);

				-- extract position of note starting at field 2
				pac_text_placeholder.position := to_position (line, 2);

			elsif kw = keyword_size then -- size 1.000
				expect_field_count (line, 2);
				pac_text_placeholder.size := to_distance (f (line, 2));

			elsif kw = keyword_linewidth then -- linewidth 0.1
				expect_field_count (line, 2);
				pac_text_placeholder.line_width := to_distance (f (line, 2));

			elsif kw = keyword_alignment then -- alignment horizontal center vertical center
				expect_field_count (line, 5);

				-- extract alignment starting at field 2
				pac_text_placeholder.alignment := to_alignment (line, 2);
				
			elsif kw = keyword_meaning then -- meaning reference, value, purpose
				expect_field_count (line, 2);
				pac_text_placeholder.meaning := to_meaning (f (line, 2));
				
			else
				invalid_keyword (kw);
			end if;
		end read_placeholder;


		
		procedure read_terminal is
			kw : constant string := f (line, 1);
		begin
			-- CS: In the following: set a corresponding parameter-found-flag
			if kw = keyword_name then -- name 1,2,H7
				expect_field_count (line, 2);
				terminal_name := to_terminal_name (f (line,2));

			elsif kw = keyword_assembly_technology then -- technology tht
				expect_field_count (line, 2);
				terminal_technology := to_assembly_technology (f (line,2));

			elsif kw = keyword_position then -- position x 12.7 y 3.0 rotation 0.0
				expect_field_count (line, 7);
				terminal_position := to_position (line,2);

			elsif kw = keyword_width_inner_layers then -- width_inner_layers 0.2
				expect_field_count (line, 2);
				tht_width_inner_layers := to_distance (f (line,2));

			elsif kw = keyword_tht_hole then -- hole drilled/milled
				expect_field_count (line, 2);
				tht_hole := to_tht_hole (f (line,2));

			elsif kw = keyword_drill_size then -- drill_size 0.8
				expect_field_count (line, 2);
				tht_drill_size := to_distance (f (line,2));
				
			elsif kw = keyword_face then -- face top/bottom
				expect_field_count (line, 2);
				smt_pad_face := to_face (f (line,2));

			elsif kw = keyword_stop_mask_status then -- stop_mask_status open/closed
				expect_field_count (line, 2);
				smt_stop_mask_status := to_stop_mask_status (f (line,2));

			elsif kw = keyword_stop_mask_shape then -- keyword_stop_mask_shape user_specific
				expect_field_count (line, 2);
				smt_stop_mask_shape := to_shape (f (line,2));
				
			elsif kw = keyword_stop_mask_shape_top then -- stop_mask_shape_top user_specific
				expect_field_count (line, 2);
				tht_stop_mask_shape_top := to_shape (f (line,2));

			elsif kw = keyword_stop_mask_shape_bottom then -- keyword_stop_mask_shape_bottom user_specific
				expect_field_count (line, 2);
				tht_stop_mask_shape_bottom := to_shape (f (line,2));

			elsif kw = keyword_solder_paste_status then -- solder_paste_status applied/none
				expect_field_count (line, 2);
				smt_solder_paste_status := to_solder_paste_status (f (line,2));

			elsif kw = keyword_solder_paste_shape then -- solder_paste_shape as_pad/shrink_pad/user_specific
				expect_field_count (line, 2);
				smt_stencil_shape := to_modification (f (line,2));

			elsif kw = keyword_solder_paste_shrink_factor then -- solder_paste_shrink_factor 0.5
				expect_field_count (line, 2);
				--smt_stencil_shrink := to_scale (f (line,2));
				smt_stencil_shrink := to_distance (f (line,2));
				
			else
				invalid_keyword (kw);
			end if;

		end read_terminal;

		
		procedure build_terminal is 
		-- Assembles the elements of a terminal and appends the final terminal to the
		-- list of terminals of the package.
			cursor : pac_terminals.cursor;
			inserted : boolean;

			-- Builds the stop mask of the terminal if it is a SMT type:
			function make_stop_mask_smt return type_stop_mask_smt is begin
				case smt_stop_mask_shape is
					when AS_PAD =>
						return (
							shape		=> AS_PAD);
						
					when EXPAND_PAD =>
						return (
							shape		=> EXPAND_PAD);
					
					when USER_SPECIFIC =>
						return (
							shape		=> USER_SPECIFIC,
							contours	=> smt_stop_mask_contours);
				end case;
			end make_stop_mask_smt;


			-- Builds the stop mask of the terminal if it is a THT type:
			function make_stop_mask_tht return type_stop_mask_tht is begin
				return r : type_stop_mask_tht do
					case tht_stop_mask_shape_top is
						when AS_PAD => 
							r.top := (shape => AS_PAD);
						
						when EXPAND_PAD =>
							r.top := (shape	=> EXPAND_PAD);
							
						when USER_SPECIFIC =>
							r.top := (shape => USER_SPECIFIC, contours => tht_stop_mask_contours_top);
					end case;

					case tht_stop_mask_shape_bottom is
						when AS_PAD => 
							r.bottom := (shape => AS_PAD);
							
						when EXPAND_PAD =>
							r.bottom := (shape => EXPAND_PAD);
							
						when USER_SPECIFIC =>
							r.bottom := (shape => USER_SPECIFIC, contours => tht_stop_mask_contours_bottom);

					end case;

				end return;
			end make_stop_mask_tht;

			
			-- Builds the stencil of the SMT pad (there is no stencil for THT pads):
			function make_stencil return type_stencil_shape is begin
				return r : type_stencil_shape do
					case smt_stencil_shape is
						when AS_PAD =>
							r := (shape => AS_PAD);
						when SHRINK_PAD =>
							r := (shape => SHRINK_PAD, shrink_factor => smt_stencil_shrink);
						when USER_SPECIFIC =>
							r := (shape => USER_SPECIFIC, contours => smt_stencil_contours);
					end case;
				end return;
			end make_stencil;

			
		begin -- build_terminal
			case terminal_technology is
				when THT => 
					case tht_hole is
						when DRILLED =>

							pac_terminals.insert (
								container	=> packge.terminals,
								key			=> terminal_name, -- 1,4,16
								position	=> cursor,
								inserted	=> inserted,
								new_item	=> (
									technology			=> THT,
									tht_hole			=> DRILLED,
									drill_size			=> tht_drill_size,
									position			=> terminal_position,
									pad_shape_tht		=> tht_pad_shape,
									stop_mask_status_tht	=> tht_stop_mask_status,
									stop_mask_shape_tht		=> make_stop_mask_tht,
									width_inner_layers		=> tht_width_inner_layers));

						when MILLED =>
							pac_terminals.insert (
								container	=> packge.terminals,
								key			=> terminal_name, -- 1,4,16
								position	=> cursor,
								inserted	=> inserted,
								new_item	=> (
									technology			=> THT,
									tht_hole			=> MILLED,
									millings			=> tht_millings,
									position			=> terminal_position,
									pad_shape_tht		=> tht_pad_shape,
									stop_mask_status_tht	=> tht_stop_mask_status,
									stop_mask_shape_tht		=> make_stop_mask_tht,
									width_inner_layers		=> tht_width_inner_layers));
					end case;

					-- clean up for next terminal
					tht_pad_shape			:= (others => <>);
					tht_hole				:= terminal_tht_hole_default;
					tht_width_inner_layers	:= type_track_width'first;
					tht_drill_size			:= type_drill_size_tht'first;

					tht_stop_mask_status			:= stop_mask_status_default;
					tht_stop_mask_shape_top			:= stop_mask_shape_default;
					tht_stop_mask_shape_bottom		:= stop_mask_shape_default;
					delete_segments (tht_stop_mask_contours_top);
					delete_segments (tht_stop_mask_contours_bottom);
					
				when SMT =>
					pac_terminals.insert (
						container	=> packge.terminals,
						key			=> terminal_name, -- 1,4,16,H9
						position	=> cursor,
						inserted	=> inserted,
						new_item	=> (
							technology				=> SMT,
							tht_hole				=> terminal_tht_hole_default, -- not relevant here, see spec
							face					=> smt_pad_face,
							position				=> terminal_position,
							pad_shape_smt			=> smt_pad_shape,
							stop_mask_status_smt	=> smt_stop_mask_status,
							stop_mask_shape_smt		=> make_stop_mask_smt,
							solder_paste_status		=> smt_solder_paste_status,
							stencil_shape			=> make_stencil
							));

					-- clean up for next terminal
					smt_stop_mask_shape		:= stop_mask_shape_default;
					delete_segments (smt_stop_mask_contours);
		 			delete_segments (smt_pad_shape);
					smt_stop_mask_status	:= stop_mask_status_default;
					smt_solder_paste_status	:= solder_paste_status_default;
					smt_stencil_shape		:= stencil_modification_default;
					delete_segments (smt_stencil_contours);
					smt_stencil_shrink		:= stencil_shrink_default;
			end case;

			if not inserted then
				log (ERROR, "terminal" & to_string (terminal_name) 
					 & " already used !", console => true);
				raise constraint_error;
			end if;

			-- clean up for next terminal
			terminal_position	:= origin_zero_rotation;
			
		end build_terminal;



		procedure build_text is begin
			case stack.parent is
				when SEC_TOP =>
					case stack.parent (degree => 2) is
						when SEC_CONDUCTOR =>
							
							append (
								container	=> packge.conductors.top.texts,
								new_item	=> (pac_text with vectorize_text (
										content			=> pac_text.content,
										size			=> pac_text.size,
										rotation		=> pac_text.position.rotation,
										position		=> pac_text.position.place,
										line_width		=> pac_text.line_width,
										alignment		=> pac_text.alignment,
										make_border		=> true,
										log_threshold	=> log_threshold + 3)));

							
						when SEC_SILKSCREEN =>
							pac_silk_texts.append (
								container	=> packge.silkscreen.top.texts,
								new_item	=> (pac_text with null record));

						when SEC_ASSEMBLY_DOCUMENTATION =>
							pac_doc_texts.append (
								container	=> packge.assy_doc.top.texts,
								new_item	=> (pac_text with null record));
							
						when SEC_STOPMASK =>
							pac_stop_texts.append (
								container	=> packge.stop_mask.top.texts,
								new_item	=> (pac_text with null record));

							
						when others => invalid_section;
					end case;

					-- clean up for next text
					pac_text := (others => <>);

					
				when SEC_BOTTOM =>
					case stack.parent (degree => 2) is
						when SEC_CONDUCTOR =>
							
							append (
								container	=> packge.conductors.bottom.texts,
								new_item	=> (pac_text with vectorize_text (
										content			=> pac_text.content,
										size			=> pac_text.size,
										rotation		=> pac_text.position.rotation,
										position		=> pac_text.position.place,
										mirror			=> MIRROR_ALONG_Y_AXIS,
										line_width		=> pac_text.line_width,
										alignment		=> pac_text.alignment,
										make_border		=> true,
										log_threshold	=> log_threshold + 3)));


						when SEC_SILKSCREEN =>
							pac_silk_texts.append (
								container	=> packge.silkscreen.bottom.texts,
								new_item	=> (pac_text with null record));


						when SEC_ASSEMBLY_DOCUMENTATION =>
							pac_doc_texts.append (
								container	=> packge.assy_doc.bottom.texts,
								new_item	=> (pac_text with null record));
							
						when SEC_STOPMASK =>
							pac_stop_texts.append (
								container	=> packge.stop_mask.bottom.texts,
								new_item	=> (pac_text with null record));

							
						when others => invalid_section;
					end case;
					
					-- clean up for next text
					pac_text := (others => <>);

				when others => invalid_section;
			end case;
		end build_text;
			
		
		procedure process_line is 

			procedure execute_section is
			-- Once a section concludes, the temporarily variables are read, evaluated
				-- and finally assembled to actual objects:

				-- fill zones
				procedure append_silk_polygon_top is begin
					pac_silk_zones.append (
						container	=> packge.silkscreen.top.zones, 
						new_item	=> (contour with null record));
					
					board_reset_contour;
				end;
				
				procedure append_silk_polygon_bottom is begin
					pac_silk_zones.append (
						container	=> packge.silkscreen.bottom.zones, 
						new_item	=> (contour with null record));
					
					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_assy_doc_polygon_top is begin
					pac_doc_zones.append (
						container	=> packge.assy_doc.top.zones, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_assy_doc_polygon_bottom is begin
					pac_doc_zones.append (
						container	=> packge.assy_doc.bottom.zones, 
						new_item	=> (contour with null record));
					
					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_keepout_polygon_top is begin
					
					pac_keepout_zones.append (
						container	=> packge.keepout.top.zones, 
						new_item	=> (contour with null record));
						
					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_keepout_polygon_bottom is begin

					pac_keepout_zones.append (
						container	=> packge.keepout.bottom.zones, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_stencil_polygon_top is begin

					pac_stencil_zones.append (
						container	=> packge.stencil.top.zones, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_stencil_polygon_bottom is begin
					pac_stencil_zones.append (
						container	=> packge.stencil.bottom.zones, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_stop_polygon_top is begin
					pac_stop_zones.append (
						container	=> packge.stop_mask.top.zones, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_stop_polygon_bottom is begin
					pac_stop_zones.append (
						container	=> packge.stop_mask.bottom.zones, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_route_restrict_zone_top is begin
					pac_route_restrict_zones.append (
						container	=> packge.route_restrict.top.zones, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_route_restrict_zone_bottom is begin
					pac_route_restrict_zones.append (
						container	=> packge.route_restrict.bottom.zones, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_via_restrict_zone_top is begin
					pac_via_restrict_zones.append (
						container	=> packge.via_restrict.top.zones, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_via_restrict_zone_bottom is begin
					pac_via_restrict_zones.append (
						container	=> packge.via_restrict.bottom.zones, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_keepout_cutout_top is begin
					pac_keepout_cutouts.append (
						container	=> packge.keepout.top.cutouts, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_keepout_cutout_bottom is begin
					pac_keepout_cutouts.append (
						container	=> packge.keepout.bottom.cutouts, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_stop_cutout_top is begin
					-- CS
					--pac_stop_cutouts.append (
						--container	=> packge.stop_mask.top.cutouts, 
						--new_item	=> contour);

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_stop_cutout_bottom is begin
					-- CS
					--pac_stop_cutouts.append (
						--container	=> packge.stop_mask.bottom.cutouts, 
						--new_item	=> contour);

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_route_restrict_cutout_top is begin
					pac_route_restrict_cutouts.append (
						container	=> packge.route_restrict.top.cutouts, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_route_restrict_cutout_bottom is begin
					pac_route_restrict_cutouts.append (
						container	=> packge.route_restrict.bottom.cutouts, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_via_restrict_cutout_top is begin
					pac_via_restrict_cutouts.append (
						container	=> packge.via_restrict.top.cutouts, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;

				
				procedure append_via_restrict_cutout_bottom is begin
					pac_via_restrict_cutouts.append (
						container	=> packge.via_restrict.bottom.cutouts, 
						new_item	=> (contour with null record));

					-- clean up for next polygon
					board_reset_contour;
				end;
			
				
				-- holes in PCB (or cutouts)
				procedure append_hole is begin
					packge.holes.append ((contour with null record));

					-- clean up for next hole
					board_reset_contour;
				end append_hole;

				
			begin -- execute_section
				case stack.current is

					when SEC_CONDUCTOR | SEC_KEEPOUT | SEC_STOPMASK | SEC_STENCIL | 
						SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
						SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT | SEC_PCB_CONTOURS_NON_PLATED | 
						SEC_TERMINALS | SEC_PACKAGE_3D_CONTOURS =>

						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;
							
					when SEC_TOP =>
						case stack.parent is
							when SEC_CONDUCTOR | SEC_KEEPOUT | SEC_STOPMASK | SEC_STENCIL | 
								SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
								SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT => null;

							when SEC_PAD_CONTOURS_THT => 
								check_outline (contour, log_threshold + 1);
								tht_pad_shape.top := contour;
								board_reset_contour;

							when SEC_STOPMASK_CONTOURS_THT =>
								check_outline (contour, log_threshold + 1);
								tht_stop_mask_contours_top := (contour with null record);
								board_reset_contour;
								
							when others => invalid_section;
						end case;
						
					when SEC_BOTTOM =>
						case stack.parent is
							when SEC_CONDUCTOR | SEC_KEEPOUT | SEC_STOPMASK | SEC_STENCIL | 
								SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
								SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT => null;

							when SEC_PAD_CONTOURS_THT =>
								check_outline (contour, log_threshold + 1);
								tht_pad_shape.bottom := contour;
								board_reset_contour;

							when SEC_STOPMASK_CONTOURS_THT =>
								check_outline (contour, log_threshold + 1);
								tht_stop_mask_contours_bottom := (contour with null record);
								board_reset_contour;
								
							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!

										et_conductor_segment.pac_conductor_lines.append (
											container	=> packge.conductors.top.lines, 
											new_item	=> (type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_SILKSCREEN => 
										pac_silk_lines.append (
											container	=> packge.silkscreen.top.lines, 
											new_item	=> (type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_ASSEMBLY_DOCUMENTATION =>
										pac_doc_lines.append (
											container	=> packge.assy_doc.top.lines, 
											new_item	=> (type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_STENCIL =>
										pac_stencil_lines.append (
											container	=> packge.stencil.top.lines, 
											new_item	=> (type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_STOPMASK =>
										pac_stop_lines.append (
											container	=> packge.stop_mask.top.lines, 
											new_item	=> (type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

										
									when SEC_ROUTE_RESTRICT =>
										pac_route_restrict_lines.append (
											container	=> packge.route_restrict.top.lines,
											new_item	=> (type_line (board_line) with null record));

										-- clean up for next line
										board_reset_line;

										
									when SEC_PAD_CONTOURS_THT => add_polygon_line (board_line);

									when SEC_STOPMASK_CONTOURS_THT => add_polygon_line (board_line);
									
									when others => invalid_section;
								end case;

								
							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!

										et_conductor_segment.pac_conductor_lines.append (
											container	=> packge.conductors.bottom.lines, 
											new_item	=> (type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_SILKSCREEN => 
										pac_silk_lines.append (
											container	=> packge.silkscreen.bottom.lines, 
											new_item	=> (type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										pac_doc_lines.append (
											container	=> packge.assy_doc.bottom.lines, 
											new_item	=> (type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;

									when SEC_STENCIL =>
										pac_stencil_lines.append (
											container	=> packge.stencil.bottom.lines, 
											new_item	=> (type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;
										
									when SEC_STOPMASK =>
										pac_stop_lines.append (
											container	=> packge.stop_mask.bottom.lines, 
											new_item	=> (type_line (board_line) with board_line_width));

										-- clean up for next line
										board_reset_line;
										board_reset_line_width;
					
										
									when SEC_ROUTE_RESTRICT =>
										pac_route_restrict_lines.append (
											container	=> packge.route_restrict.bottom.lines,
											new_item	=> (type_line (board_line) with null record));

										-- clean up for next line
										board_reset_line;

										
									when SEC_PAD_CONTOURS_THT => add_polygon_line (board_line);

									when SEC_STOPMASK_CONTOURS_THT => add_polygon_line (board_line);
									
									when others => invalid_section;
								end case;
								
							when SEC_HOLE => add_polygon_line (board_line);								
							when SEC_PAD_CONTOURS_SMT => add_polygon_line (board_line);
							when SEC_STENCIL_CONTOURS => add_polygon_line (board_line);							
							when SEC_STOPMASK_CONTOURS_SMT => add_polygon_line (board_line);							
							when SEC_MILLINGS => add_polygon_line (board_line);
							when SEC_CONTOURS => add_polygon_line (board_line);								
							when others => invalid_section;
						end case;

						
					when SEC_ARC =>
						board_check_arc (log_threshold + 1);
						
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!

										et_conductor_segment.pac_conductor_arcs.append (
											container	=> packge.conductors.top.arcs, 
											new_item	=> (type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_SILKSCREEN => 
										pac_silk_arcs.append (
											container	=> packge.silkscreen.top.arcs, 
											new_item	=> (type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_ASSEMBLY_DOCUMENTATION =>
										pac_doc_arcs.append (
											container	=> packge.assy_doc.top.arcs, 
											new_item	=> (type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_STENCIL =>
										pac_stencil_arcs.append (
											container	=> packge.stencil.top.arcs, 
											new_item	=> (type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_STOPMASK =>
										pac_stop_arcs.append (
											container	=> packge.stop_mask.top.arcs, 
											new_item	=> (type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

										
									when SEC_ROUTE_RESTRICT =>										
										pac_route_restrict_arcs.append (
											container	=> packge.route_restrict.top.arcs,
											new_item	=> (type_arc (board_arc) with null record));

										-- clean up for next arc
										board_reset_arc;

										
									when SEC_PAD_CONTOURS_THT => add_polygon_arc (board_arc);
									when SEC_STOPMASK_CONTOURS_THT => add_polygon_arc (board_arc);										
									when others => invalid_section;
								end case;

								
							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!

										et_conductor_segment.pac_conductor_arcs.append (
											container	=> packge.conductors.bottom.arcs, 
											new_item	=> (type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_SILKSCREEN => 
										pac_silk_arcs.append (
											container	=> packge.silkscreen.bottom.arcs, 
											new_item	=> (type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										pac_doc_arcs.append (
											container	=> packge.assy_doc.bottom.arcs, 
											new_item	=> (type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

									when SEC_STENCIL =>
										pac_stencil_arcs.append (
											container	=> packge.stencil.bottom.arcs, 
											new_item	=> (type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;
										
									when SEC_STOPMASK =>
										pac_stop_arcs.append (
											container	=> packge.stop_mask.bottom.arcs, 
											new_item	=> (type_arc (board_arc) with board_line_width));

										-- clean up for next arc
										board_reset_arc;
										board_reset_line_width;

										
									when SEC_ROUTE_RESTRICT =>										
										pac_route_restrict_arcs.append (
											container	=> packge.route_restrict.bottom.arcs,
											new_item	=> (type_arc (board_arc) with null record));

										-- clean up for next arc
										board_reset_arc;

									
									when SEC_PAD_CONTOURS_THT => add_polygon_arc (board_arc);
									when SEC_STOPMASK_CONTOURS_THT => add_polygon_arc (board_arc);									
									when others => invalid_section;
								end case;

							when SEC_HOLE => add_polygon_arc (board_arc);
							when SEC_PAD_CONTOURS_SMT => add_polygon_arc (board_arc);
							when SEC_STENCIL_CONTOURS => add_polygon_arc (board_arc);							
							when SEC_STOPMASK_CONTOURS_SMT => add_polygon_arc (board_arc);							
							when SEC_MILLINGS => add_polygon_arc (board_arc);
							when SEC_CONTOURS => add_polygon_arc (board_arc);								
							when others => invalid_section;
						end case;

						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!

										et_conductor_segment.pac_conductor_circles.append (
											container	=> packge.conductors.top.circles, 
											new_item	=> (type_circle (board_circle) with board_line_width));
										
									when SEC_SILKSCREEN => 
										pac_silk_circles.append (
											container	=> packge.silkscreen.top.circles, 
											new_item	=> (type_circle (board_circle) with board_line_width));
															
										board_reset_circle; -- clean up for next circle

									when SEC_ASSEMBLY_DOCUMENTATION =>
										pac_doc_circles.append (
											container	=> packge.assy_doc.top.circles, 
											new_item	=> (type_circle (board_circle) with board_line_width));

										board_reset_circle; -- clean up for next circle
										
									when SEC_STENCIL =>
										pac_stencil_circles.append (
											container	=> packge.stencil.top.circles, 
											new_item	=> (type_circle (board_circle) with board_line_width));

										board_reset_circle; -- clean up for next circle
										
									when SEC_STOPMASK =>
										pac_stop_circles.append (
											container	=> packge.stop_mask.top.circles, 
											new_item	=> (type_circle (board_circle) with board_line_width));

										board_reset_circle; -- clean up for next circle

										
									when SEC_ROUTE_RESTRICT =>										
										pac_route_restrict_circles.append (
											container	=> packge.route_restrict.top.circles,
											new_item	=> (type_circle (board_circle) with null record));

										board_reset_circle; -- clean up for next circle

								
									when SEC_PAD_CONTOURS_THT => add_polygon_circle (board_circle);
									when SEC_STOPMASK_CONTOURS_THT => add_polygon_circle (board_circle);									
									when others => invalid_section;
								end case;

								
							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!

										et_conductor_segment.pac_conductor_circles.append (
											container	=> packge.conductors.bottom.circles, 
											new_item	=> (type_circle (board_circle) with board_line_width));

									when SEC_SILKSCREEN => 
										pac_silk_circles.append (
											container	=> packge.silkscreen.bottom.circles, 
											new_item	=> (type_circle (board_circle) with board_line_width));

										board_reset_circle; -- clean up for next circle
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										pac_doc_circles.append (
											container	=> packge.assy_doc.bottom.circles, 
											new_item	=> (type_circle (board_circle) with board_line_width));

										board_reset_circle; -- clean up for next circle

									when SEC_STENCIL =>
										pac_stencil_circles.append (
											container	=> packge.stencil.bottom.circles, 
											new_item	=> (type_circle (board_circle) with board_line_width));

										board_reset_circle; -- clean up for next circle

									when SEC_STOPMASK =>
										pac_stop_circles.append (
											container	=> packge.stop_mask.bottom.circles, 
											new_item	=> (type_circle (board_circle) with board_line_width));

										board_reset_circle; -- clean up for next circle

										
									when SEC_ROUTE_RESTRICT =>										
										pac_route_restrict_circles.append (
											container	=> packge.route_restrict.bottom.circles,
											new_item	=> (type_circle (board_circle) with null record));

										board_reset_circle; -- clean up for next circle

										
									when SEC_PAD_CONTOURS_THT => add_polygon_circle (board_circle);
									when SEC_STOPMASK_CONTOURS_THT => add_polygon_circle (board_circle);									
									when others => invalid_section;
								end case;

							when SEC_HOLE =>
								add_polygon_circle (board_circle);
								
							when SEC_PAD_CONTOURS_SMT => add_polygon_circle (board_circle);
							when SEC_STENCIL_CONTOURS => add_polygon_circle (board_circle);							
							when SEC_STOPMASK_CONTOURS_SMT => add_polygon_circle (board_circle);							
							when SEC_MILLINGS => add_polygon_circle (board_circle);
							when SEC_CONTOURS => add_polygon_circle (board_circle);								
							when others => invalid_section;
						end case;

						
					when SEC_ZONE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>
										append_silk_polygon_top;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_polygon_top;
										
									when SEC_STENCIL =>
										append_stencil_polygon_top;
										
									when SEC_STOPMASK =>
										append_stop_polygon_top;
										
									when SEC_KEEPOUT =>
										append_keepout_polygon_top;

									when SEC_ROUTE_RESTRICT =>
										append_route_restrict_zone_top;

									when SEC_VIA_RESTRICT =>
										append_via_restrict_zone_top;
										
									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>
										append_silk_polygon_bottom;
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										append_assy_doc_polygon_bottom;
										
									when SEC_STENCIL =>
										append_stencil_polygon_bottom;
										
									when SEC_STOPMASK =>
										append_stop_polygon_bottom;
										
									when SEC_KEEPOUT =>
										append_keepout_polygon_bottom;

									when SEC_ROUTE_RESTRICT =>
										append_route_restrict_zone_bottom;

									when SEC_VIA_RESTRICT =>
										append_via_restrict_zone_bottom;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_STOPMASK =>
										append_stop_cutout_top;
										
									when SEC_KEEPOUT =>
										append_keepout_cutout_top;

									when SEC_ROUTE_RESTRICT =>
										append_route_restrict_cutout_top;

									when SEC_VIA_RESTRICT =>
										append_via_restrict_cutout_top;

									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_STOPMASK =>
										append_stop_cutout_bottom;
										
									when SEC_KEEPOUT =>
										append_keepout_cutout_bottom;

									when SEC_ROUTE_RESTRICT =>
										append_route_restrict_cutout_bottom;

									when SEC_VIA_RESTRICT =>
										append_via_restrict_cutout_bottom;
										
									when others => invalid_section;
								end case;					
								
							when others => invalid_section;
						end case;
						
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_ZONE => check_outline (contour, log_threshold + 1);
							when SEC_CUTOUT_ZONE => check_outline (contour, log_threshold + 1);
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						build_text;

					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_TOP =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>
										
										pac_text_placeholders.append (
											container	=> packge.silkscreen.top.placeholders,
											new_item	=> pac_text_placeholder);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										
										pac_text_placeholders.append (
											container	=> packge.assy_doc.top.placeholders,
											new_item	=> pac_text_placeholder);
										
									when others => invalid_section;
								end case;

								-- clean up for next placeholder
								pac_text_placeholder := (others => <>);

								
							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>
										
										pac_text_placeholders.append (
											container	=> packge.silkscreen.bottom.placeholders,
											new_item	=> pac_text_placeholder);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										
										pac_text_placeholders.append (
											container	=> packge.assy_doc.bottom.placeholders,
											new_item	=> pac_text_placeholder);
										
									when others => invalid_section;
								end case;

								-- clean up for next placeholder
								pac_text_placeholder := (others => <>);
								
							when others => invalid_section;
						end case;

						
					when SEC_TERMINAL =>
						case stack.parent is
							when SEC_TERMINALS => 
								-- Now all elements of the terminal have been read
								-- and can be assembled to the final terminal:
								build_terminal;
								
							when others => invalid_section;
						end case;

					when SEC_PAD_CONTOURS_SMT =>
						case stack.parent is
							when SEC_TERMINAL => 
								check_outline (contour, log_threshold + 1);
								smt_pad_shape := contour;
								board_reset_contour;
								
							when others => invalid_section;
						end case;

					when SEC_STENCIL_CONTOURS =>
						case stack.parent is
							when SEC_TERMINAL => 
								check_outline (contour, log_threshold + 1);
								smt_stencil_contours := (contour with null record);
								board_reset_contour;
								
							when others => invalid_section;
						end case;
						
					when SEC_PAD_CONTOURS_THT =>
						case stack.parent is
							when SEC_TERMINAL => null;
							when others => invalid_section;
						end case;

					when SEC_STOPMASK_CONTOURS_SMT =>
						case stack.parent is
							when SEC_TERMINAL =>
								check_outline (contour, log_threshold + 1);
								smt_stop_mask_contours := (contour with null record);
								board_reset_contour;
								
							when others => invalid_section;
						end case;

					when SEC_STOPMASK_CONTOURS_THT =>
						case stack.parent is
							when SEC_TERMINAL => null;
							when others => invalid_section;
						end case;
						
					when SEC_MILLINGS =>
						case stack.parent is
							when SEC_TERMINAL =>
								check_outline (contour, log_threshold + 1);
								tht_millings := contour;
								board_reset_contour;
								
							when others => invalid_section;
						end case;

					when SEC_HOLE =>
						case stack.parent is
							when SEC_PCB_CONTOURS_NON_PLATED =>
								append_hole;
							
							when others => invalid_section;
						end case;
						
					when SEC_INIT => raise constraint_error;
						
				end case;

			end execute_section;

			
			-- Tests if the current line is a section header or footer. Returns true in both cases.
			-- Returns false if the current line is neither a section header or footer.
			-- If it is a header, the section name is pushed onto the sections stack.
			-- If it is a footer, the latest section name is popped from the stack.
			function set (
				section_keyword	: in string; -- [SILKSCREEN
				section			: in type_package_section) -- SEC_ZONE
				return boolean is 
			begin -- set
				if f (line, 1) = section_keyword then -- section name detected in field 1
					if f (line, 2) = section_begin then -- section header detected in field 2
						stack.push (section);
						log (text => write_enter_section & to_string (section), level => log_threshold + 3);
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
						execute_section;
						
						stack.pop;
						if stack.empty then
							log (text => write_top_level_reached, level => log_threshold + 3);
						else
							log (text => write_return_to_section & to_string (stack.current), level => log_threshold + 3);
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

			
		begin -- process_line
			if set (section_top, SEC_TOP) then null;			
			elsif set (section_bottom, SEC_BOTTOM) then null;								
			elsif set (section_line, SEC_LINE) then null;
			elsif set (section_arc, SEC_ARC) then null;
			elsif set (section_circle, SEC_CIRCLE) then null;
			elsif set (section_silkscreen, SEC_SILKSCREEN) then null;
			elsif set (section_assembly_doc, SEC_ASSEMBLY_DOCUMENTATION) then null;
			elsif set (section_keepout, SEC_KEEPOUT) then null;			
			elsif set (section_conductor, SEC_CONDUCTOR) then null;
			elsif set (section_stopmask, SEC_STOPMASK) then null;			
			elsif set (section_stencil, SEC_STENCIL) then null;			
			elsif set (section_route_restrict, SEC_ROUTE_RESTRICT) then null;			
			elsif set (section_via_restrict, SEC_VIA_RESTRICT) then null;
			elsif set (section_pcb_contours, SEC_PCB_CONTOURS_NON_PLATED) then null;
			elsif set (section_hole, SEC_HOLE) then null;
			elsif set (section_pad_contours_smt, SEC_PAD_CONTOURS_SMT) then null;
			elsif set (section_pad_contours_tht, SEC_PAD_CONTOURS_THT) then null;
			elsif set (section_stencil_contours, SEC_STENCIL_CONTOURS) then null;
			elsif set (section_stopmask_contours_smt, SEC_STOPMASK_CONTOURS_SMT) then null;
			elsif set (section_stopmask_contours_tht, SEC_STOPMASK_CONTOURS_THT) then null;
			elsif set (section_pad_millings, SEC_MILLINGS) then null;			
			elsif set (section_text, SEC_TEXT) then null;
			elsif set (section_placeholder, SEC_PLACEHOLDER) then null;
			elsif set (section_terminals, SEC_TERMINALS) then null;
			elsif set (section_terminal, SEC_TERMINAL) then null;
			elsif set (section_zone, SEC_ZONE) then null;
			elsif set (section_contours, SEC_CONTOURS) then null;
			elsif set (section_cutout_zone, SEC_CUTOUT_ZONE) then null;
			else
				-- The line contains something else -> the payload data. 
				-- Temporarily this data is stored in corresponding variables.

				log (text => "package line --> " & to_string (line), level => log_threshold + 3);
		
				case stack.current is

					when SEC_INIT =>
						declare
							kw : string := f (line, 1);
						begin
							-- CS: In the following: set a corresponding parameter-found-flag
							if kw = keyword_bom_relevant then -- bom_relevant yes/no
								expect_field_count (line, 2);
								pac_appearance := to_bom_relevant (f (line,2));

								-- Depending on the appearance we create a virtual or real package
								-- where pointer packge is pointing at:
								case pac_appearance is
									when BOM_RELEVANT_YES =>
										packge := new type_package_model' (
													appearance	=> BOM_RELEVANT_YES,
													others		=> <>);

									when BOM_RELEVANT_NO =>
										packge := new type_package_model' (
													appearance	=> BOM_RELEVANT_NO,
													others		=> <>);
								end case;
										
							elsif kw = keyword_description then -- description "blabla"
								expect_field_count (line, 2);
								pac_description := to_package_description (f (line,2));

							elsif kw = keyword_assembly_technology then -- technology SMT/THT
								expect_field_count (line, 2);
								pac_technology := to_assembly_technology (f (line,2));
								
							else
								invalid_keyword (kw);
							end if;
						end;

					when SEC_CONDUCTOR | SEC_KEEPOUT | SEC_STOPMASK | SEC_STENCIL | 
						SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
						SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT | SEC_PCB_CONTOURS_NON_PLATED | 
						SEC_TERMINALS | SEC_PACKAGE_3D_CONTOURS =>

						case stack.parent is
							when SEC_INIT => null;
							when others => invalid_section;
						end case;

					when SEC_STOPMASK_CONTOURS_THT | SEC_STOPMASK_CONTOURS_SMT =>
						case stack.parent is
							when SEC_TERMINAL => null;
							when others => invalid_section;
						end case;
						
					when SEC_TOP | SEC_BOTTOM =>
						case stack.parent is
							when SEC_CONDUCTOR | SEC_KEEPOUT | SEC_STOPMASK | SEC_STENCIL | 
								SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
								SEC_PAD_CONTOURS_THT | 
								SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT => null;

							when SEC_STOPMASK_CONTOURS_THT => null;								
							when others => invalid_section;
						end case;
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR | SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>

										if not read_board_line (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_PAD_CONTOURS_THT => read_board_line (line);
									when SEC_STOPMASK_CONTOURS_THT => read_board_line (line);
									--when SEC_VIA_RESTRICT => read_board_line (line);
									when others => invalid_section;
								end case;

							when SEC_HOLE => read_board_line (line);								
							when SEC_PAD_CONTOURS_SMT => read_board_line (line);							
							when SEC_STENCIL_CONTOURS => read_board_line (line);							
							when SEC_STOPMASK_CONTOURS_SMT => read_board_line (line);							
							when SEC_MILLINGS => read_board_line (line);
							when SEC_CONTOURS => read_board_line (line);								
							when others => invalid_section;
						end case;
						
					when SEC_ARC =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR | SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>

										if not read_board_arc (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_PAD_CONTOURS_THT => read_board_arc (line);
									when SEC_STOPMASK_CONTOURS_THT => read_board_arc (line);
									when others => invalid_section;
								end case;

							when SEC_HOLE => read_board_arc (line);
							when SEC_PAD_CONTOURS_SMT => read_board_arc (line);
							when SEC_STENCIL_CONTOURS => read_board_arc (line);
							when SEC_STOPMASK_CONTOURS_SMT => read_board_arc (line);
							when SEC_MILLINGS => read_board_arc (line);
							when SEC_CONTOURS => read_board_arc (line);								
							when others => invalid_section;
						end case;

					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
										
										if not read_board_circle (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));

												elsif kw = keyword_filled then -- filled yes/no
													expect_field_count (line, 2);													
													board_filled := to_filled (f (line, 2));

												elsif kw = keyword_fill_style then -- fill_style solid/hatched
													expect_field_count (line, 2);													
													board_fill_style := to_fill_style (f (line, 2));
													
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
										
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!
										if not read_board_circle (line) then
											declare
												kw : string := f (line, 1);
											begin
												-- CS: In the following: set a corresponding parameter-found-flag
												if kw = keyword_width then -- width 0.5
													expect_field_count (line, 2);
													board_line_width := to_distance (f (line, 2));
												else
													invalid_keyword (kw);
												end if;
											end;
										end if;
										
									when SEC_PAD_CONTOURS_THT => read_board_circle (line);
									when SEC_STOPMASK_CONTOURS_THT => read_board_circle (line);									
									when others => invalid_section;
								end case;

							when SEC_HOLE => read_board_circle (line);
							when SEC_PAD_CONTOURS_SMT => read_board_circle (line);
							when SEC_STENCIL_CONTOURS => read_board_circle (line);
							when SEC_STOPMASK_CONTOURS_SMT => read_board_circle (line);
							when SEC_MILLINGS => read_board_circle (line);
							when SEC_CONTOURS => read_board_circle (line);
							when others => invalid_section;
						end case;

					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK =>
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_easing_style then -- corner_easing none/chamfer/fillet
												expect_field_count (line, 2);													
												board_easing.style := to_easing_style (f (line, 2));

											elsif kw = keyword_easing_radius then -- easing_radius 0.4
												expect_field_count (line, 2);													
												board_easing.radius := to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when SEC_KEEPOUT |  SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
										-- no parameters allowed here
										declare
											kw : string := f (line, 1);
										begin
											invalid_keyword (kw);
										end;
										
									when SEC_CONDUCTOR =>
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_easing_style then -- corner_easing none/chamfer/fillet
												expect_field_count (line, 2);													
												board_easing.style := to_easing_style (f (line, 2));

											elsif kw = keyword_easing_radius then -- easing_radius 0.4
												expect_field_count (line, 2);													
												board_easing.radius := to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when others => invalid_section;
								end case;
										
							when others => invalid_section;
						end case;
						
					when SEC_ZONE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STENCIL | SEC_STOPMASK =>
										null;

									when SEC_KEEPOUT | SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT =>
										null;
										
									when SEC_CONDUCTOR =>
										declare
											kw : string := f (line, 1);
										begin
											-- CS: In the following: set a corresponding parameter-found-flag
											if kw = keyword_fill_style then -- fill_style solid/hatched
												expect_field_count (line, 2);													
												board_fill_style := to_fill_style (f (line, 2));

											elsif kw = keyword_easing_style then -- corner_easing none/chamfer/fillet
												expect_field_count (line, 2);													
												board_easing.style := to_easing_style (f (line, 2));

											elsif kw = keyword_easing_radius then -- easing_radius 0.4
												expect_field_count (line, 2);													
												board_easing.radius := to_distance (f (line, 2));
												
											elsif kw = keyword_spacing then -- spacing 0.3
												expect_field_count (line, 2);													
												fill_spacing := to_distance (f (line, 2));

											elsif kw = keyword_isolation then -- isolation 0.5
												expect_field_count (line, 2);
												polygon_isolation := to_distance (f (line, 2));

											elsif kw = keyword_width then -- width 0.5
												expect_field_count (line, 2);
												polygon_width_min := to_distance (f (line, 2));
												
											else
												invalid_keyword (kw);
											end if;
										end;

									when others => invalid_section;
								end case;
								
							when others => invalid_section;
						end case;
						
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_ZONE => null;
							when SEC_CUTOUT_ZONE => null;
							when others => invalid_section;
						end case;

					when SEC_TEXT =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR |
										SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
										SEC_STOPMASK =>

										read_text;
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
								
						end case;

					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION =>

										read_placeholder;

									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

					when SEC_TERMINAL =>
						case stack.parent is
							when SEC_TERMINALS => read_terminal;
							when others => invalid_section;
						end case;

					when SEC_PAD_CONTOURS_SMT | SEC_STENCIL_CONTOURS | SEC_PAD_CONTOURS_THT | SEC_MILLINGS =>
						case stack.parent is
							when SEC_TERMINAL => null;
							when others => invalid_section;
						end case;

					when SEC_HOLE =>
						case stack.parent is
							when SEC_PCB_CONTOURS_NON_PLATED => null;
							when others => invalid_section;
						end case;
						
				end case;
			end if;

			
			exception when event: others =>
				log (text => "file " & to_string (file_name) & space 
					 & get_affected_line (line) & to_string (line), console => true);
				raise;
			
		end process_line;
		
		previous_input : ada.text_io.file_type renames current_input;

		use et_pcb_stack;

		
	begin -- read_package
		log_indentation_up;
		log (text => "reading package " & to_string (file_name) & " ...", level => log_threshold);

		if check_layers.check = YES then
			log (text => " with signal layer check. Deepest allowed layer is " &
				 to_string (check_layers.deepest_layer), level => log_threshold);
		end if;
		
		log_indentation_up;
		
		-- test if container et_pcb.packages already contains the package
		-- named "file_name". If so, there would be no need to read the file_name again.
		if pac_package_models.contains (package_library, file_name) then
			log (text => "already read -> skipped", level => log_threshold + 1);
		else
			
			-- open package file
			open (
				file => file_handle,
				mode => in_file, 
				name => et_directory_and_file_ops.expand (to_string (file_name)));

			set_input (file_handle);
			
			-- Init section stack.
			stack.init;
			stack.push (SEC_INIT);

			-- read the file line by line
			while not end_of_file loop
				line := read_line (
					line 			=> get_line,
					number			=> positive (ada.text_io.line (current_input)),
					comment_mark 	=> comment_mark,
					delimiter_wrap	=> true, -- strings are enclosed in quotations
					ifs 			=> space); -- fields are separated by space

				-- we are interested in lines that contain something. emtpy lines are skipped:
				if get_field_count (line) > 0 then
					process_line;
				end if;
			end loop;

			-- As a safety measure the top section must be reached finally.
			if stack.depth > 1 then 
				log (WARNING, write_section_stack_not_empty);
			end if;

			set_input (previous_input);
			close (file_handle);

			-- Assign description and technology as they have been read earlier.
			packge.description := pac_description;
			packge.technology := pac_technology;

			-- Insert the package (accessed by pointer packge) in et_pcb.packages:
			pac_package_models.insert (
				container	=> package_library, 
				key			=> file_name, -- libraries/packages/S_SO14.pac
				new_item	=> packge.all);

		end if;

		-- CS Check integrity of package (style guides, conventions ...)
		-- use function "last" to fetch latest package

		log_indentation_down;
		log_indentation_down;		

		exception when event: others =>
			if is_open (file_handle) then 
				set_input (previous_input);
				close (file_handle); 
			end if;
			raise;

	end read_package;

	
	
end et_package_read;
