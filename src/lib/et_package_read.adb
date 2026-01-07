------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                             PACKAGE READ                                 --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2026                                                --
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


with et_package_read_hole;				use et_package_read_hole;
with et_package_read_assy_doc;			use et_package_read_assy_doc;
with et_package_read_silkscreen;		use et_package_read_silkscreen;
with et_package_read_stencil;			use et_package_read_stencil;
with et_package_read_stopmask;			use et_package_read_stopmask;
with et_package_read_keepout;			use et_package_read_keepout;
with et_package_read_conductors;		use et_package_read_conductors;
with et_package_read_route_restrict;	use et_package_read_route_restrict;
with et_package_read_contour;			use et_package_read_contour;
with et_package_read_via_restrict;		use et_package_read_via_restrict;
with et_package_read_terminal;			use et_package_read_terminal;
with et_package_read_text;				use et_package_read_text;



package body et_package_read is

	use pac_text_board_vectorized;
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
		-- packge	: access type_package_model; -- CS rename to package_model
		packge	: type_package_model_access; -- CS rename to package_model
		
		pac_appearance			: type_bom_relevant := bom_relevant_default;

		-- The description and technology will be assigned once the complete
		-- model has been read. See main of this procedure.
		pac_description			: pac_package_description.bounded_string; 
		pac_technology			: type_assembly_technology := assembly_technology_default;

	

		

		



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
							insert_silk_text (packge, TOP, log_threshold + 2);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_doc_text (packge, TOP, log_threshold + 2);
							
						when SEC_STOPMASK =>
							insert_stop_text (packge, TOP, log_threshold + 2);
							
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
							insert_silk_text (packge, BOTTOM, log_threshold + 2);

						when SEC_ASSEMBLY_DOCUMENTATION =>
							insert_doc_text (packge, BOTTOM, log_threshold + 2);
							
						when SEC_STOPMASK =>
							insert_stop_text (packge, BOTTOM, log_threshold + 2);
							
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
			begin
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
								assign_contour_conductor_tht (TOP);

							when SEC_STOPMASK_CONTOURS_THT =>
								assign_contour_stopmask_tht (TOP);
								
							when others => invalid_section;
						end case;
						
						
					when SEC_BOTTOM =>
						case stack.parent is
							when SEC_CONDUCTOR | SEC_KEEPOUT | SEC_STOPMASK | SEC_STENCIL | 
								SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION |
								SEC_ROUTE_RESTRICT | SEC_VIA_RESTRICT => null;

							when SEC_PAD_CONTOURS_THT => 
								assign_contour_conductor_tht (TOP);

							when SEC_STOPMASK_CONTOURS_THT =>
								assign_contour_stopmask_tht (TOP);
								
							when others => invalid_section;
						end case;
						
						
					when SEC_LINE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!
										insert_conductor_line (packge, TOP, log_threshold);

									when SEC_SILKSCREEN => 
										insert_silk_line (packge, TOP, log_threshold);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_doc_line (packge, TOP, log_threshold);
										
									when SEC_STENCIL =>
										insert_stencil_line (packge, TOP, log_threshold);

									when SEC_STOPMASK =>
										insert_stop_line (packge, TOP, log_threshold);
										
									when SEC_ROUTE_RESTRICT =>
										insert_route_restrict_line (packge, TOP, log_threshold);
										
									when SEC_PAD_CONTOURS_THT | SEC_STOPMASK_CONTOURS_THT => 
										insert_contour_line;
									
									when others => invalid_section;
								end case;

								
							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!
										insert_conductor_line (packge, BOTTOM, log_threshold);

									when SEC_SILKSCREEN => 
										insert_silk_line (packge, BOTTOM, log_threshold);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_doc_line (packge, BOTTOM, log_threshold);

									when SEC_STENCIL =>
										insert_stencil_line (packge, BOTTOM, log_threshold);
										
									when SEC_STOPMASK =>
										insert_stop_line (packge, BOTTOM, log_threshold);
										
									when SEC_ROUTE_RESTRICT =>
										insert_route_restrict_line (packge, BOTTOM, log_threshold);
										
									when SEC_PAD_CONTOURS_THT | SEC_STOPMASK_CONTOURS_THT => 
										insert_contour_line;
									
									when others => invalid_section;
								end case;
							
							
							when SEC_HOLE | SEC_PAD_CONTOURS_SMT | SEC_STENCIL_CONTOURS
								| SEC_STOPMASK_CONTOURS_SMT | SEC_MILLINGS | SEC_CONTOURS => 
								insert_contour_line;
								
							when others => invalid_section;
						end case;

						
					when SEC_ARC =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!
										insert_conductor_arc (packge, TOP, log_threshold);

									when SEC_SILKSCREEN => 
										insert_silk_arc (packge, TOP, log_threshold);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_doc_arc (packge, TOP, log_threshold);

									when SEC_STENCIL =>
										insert_stencil_arc (packge, TOP, log_threshold);

									when SEC_STOPMASK =>
										insert_stop_arc (packge, TOP, log_threshold);
																				
									when SEC_ROUTE_RESTRICT =>										
										insert_route_restrict_arc (packge, TOP, log_threshold);
									
									when SEC_PAD_CONTOURS_THT | SEC_STOPMASK_CONTOURS_THT => 
										insert_contour_arc;

									when others => invalid_section;
								end case;

								
							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!
										insert_conductor_arc (packge, BOTTOM, log_threshold);

									when SEC_SILKSCREEN => 
										insert_silk_arc (packge, BOTTOM, log_threshold);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_doc_arc (packge, BOTTOM, log_threshold);

									when SEC_STENCIL =>
										insert_stencil_arc (packge, BOTTOM, log_threshold);
										
									when SEC_STOPMASK =>
										insert_stop_arc (packge, BOTTOM, log_threshold);
										
									when SEC_ROUTE_RESTRICT =>										
										insert_route_restrict_arc (packge, BOTTOM, log_threshold);
									
									when SEC_PAD_CONTOURS_THT | SEC_STOPMASK_CONTOURS_THT => 
										insert_contour_arc;
									
									when others => invalid_section;
								end case;

								
							when SEC_HOLE | SEC_PAD_CONTOURS_SMT | SEC_STENCIL_CONTOURS
								| SEC_STOPMASK_CONTOURS_SMT | SEC_MILLINGS | SEC_CONTOURS => 
								insert_contour_arc;
								
							when others => invalid_section;
						end case;

						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!
										insert_conductor_circle (packge, TOP, log_threshold);
										
									when SEC_SILKSCREEN => 
										insert_silk_circle (packge, TOP, log_threshold);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_doc_circle (packge, TOP, log_threshold);
										
									when SEC_STENCIL =>
										insert_stencil_circle (packge, TOP, log_threshold);
										
									when SEC_STOPMASK =>
										insert_stop_circle (packge, TOP, log_threshold);
										
									when SEC_ROUTE_RESTRICT =>										
										insert_route_restrict_circle (packge, TOP, log_threshold);
									
									when SEC_PAD_CONTOURS_THT | SEC_STOPMASK_CONTOURS_THT => 
										insert_contour_circle;									
									
									when others => invalid_section;
								end case;

								
							when SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_CONDUCTOR => -- NON-ELECTRIC !!
										insert_conductor_circle (packge, BOTTOM, log_threshold);

									when SEC_SILKSCREEN => 
										insert_silk_circle (packge, BOTTOM, log_threshold);

									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_doc_circle (packge, BOTTOM, log_threshold);
										
									when SEC_STENCIL =>
										insert_stencil_circle (packge, BOTTOM, log_threshold);

									when SEC_STOPMASK =>
										insert_stop_circle (packge, BOTTOM, log_threshold);
										
									when SEC_ROUTE_RESTRICT =>										
										insert_route_restrict_circle (packge, BOTTOM, log_threshold);

									when SEC_PAD_CONTOURS_THT | SEC_STOPMASK_CONTOURS_THT => 
										insert_contour_circle;									

									when others => invalid_section;
								end case;

						
							when SEC_HOLE | SEC_PAD_CONTOURS_SMT | SEC_STENCIL_CONTOURS
								| SEC_STOPMASK_CONTOURS_SMT | SEC_MILLINGS | SEC_CONTOURS => 
								insert_contour_circle;
								
							when others => invalid_section;
						end case;

						
					when SEC_ZONE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>
										insert_silk_zone (packge, TOP, log_threshold);
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_doc_zone (packge, TOP, log_threshold);
										
									when SEC_STENCIL =>
										insert_stencil_zone (packge, TOP, log_threshold);
										
									when SEC_STOPMASK =>
										insert_stop_zone (packge, TOP, log_threshold);
										
									when SEC_KEEPOUT =>
										insert_keepout_zone (packge, TOP, log_threshold);

									when SEC_ROUTE_RESTRICT =>
										insert_route_restrict_zone (packge, TOP, log_threshold);

									when SEC_VIA_RESTRICT =>
										insert_via_restrict_zone (packge, TOP, log_threshold);
										
									when others => invalid_section;
								end case;
								

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN =>
										insert_silk_zone (packge, BOTTOM, log_threshold);
										
									when SEC_ASSEMBLY_DOCUMENTATION =>
										insert_doc_zone (packge, BOTTOM, log_threshold);
										
									when SEC_STENCIL =>
										insert_stencil_zone (packge, BOTTOM, log_threshold);
										
									when SEC_STOPMASK =>
										insert_stop_zone (packge, BOTTOM, log_threshold);
										
									when SEC_KEEPOUT =>
										insert_keepout_zone (packge, BOTTOM, log_threshold);

									when SEC_ROUTE_RESTRICT =>
										insert_route_restrict_zone (packge, BOTTOM, log_threshold);

									when SEC_VIA_RESTRICT =>
										insert_via_restrict_zone (packge, BOTTOM, log_threshold);
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;
						

					when SEC_CUTOUT_ZONE =>
						case stack.parent is
							when SEC_TOP => 
								case stack.parent (degree => 2) is
									when SEC_KEEPOUT =>
										insert_keepout_zone_cutout (packge, TOP, log_threshold);

									when SEC_ROUTE_RESTRICT =>
										insert_route_restrict_zone_cutout (packge, TOP, log_threshold);

									when SEC_VIA_RESTRICT =>
										insert_via_restrict_zone_cutout (packge, TOP, log_threshold);

									when others => invalid_section;
								end case;

							when SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_KEEPOUT =>
										insert_keepout_zone_cutout (packge, BOTTOM, log_threshold);

									when SEC_ROUTE_RESTRICT =>
										insert_route_restrict_zone_cutout (packge, BOTTOM, log_threshold);

									when SEC_VIA_RESTRICT =>
										insert_via_restrict_zone_cutout (packge, BOTTOM, log_threshold);
										
									when others => invalid_section;
								end case;					
								
							when others => invalid_section;
						end case;
						
						
					when SEC_CONTOURS =>
						case stack.parent is
							when SEC_ZONE => 
								null; -- CS
								-- check_outline (contour, log_threshold + 1);
								
							when SEC_CUTOUT_ZONE => 
								null; -- CS
								-- check_outline (contour, log_threshold + 1);
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
								build_terminal (packge, log_threshold);
								
							when others => invalid_section;
						end case;

						
					when SEC_PAD_CONTOURS_SMT =>
						case stack.parent is
							when SEC_TERMINAL =>
								assign_contour_conductor_smt;
								
							when others => invalid_section;
						end case;

						
					when SEC_STENCIL_CONTOURS =>
						case stack.parent is
							when SEC_TERMINAL => 
								assign_contour_stencil_smt;
								
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
								assign_contour_stopmask_smt;
								
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
								assign_plated_millings;
								
							when others => invalid_section;
						end case;

						
					when SEC_HOLE =>
						case stack.parent is
							when SEC_PCB_CONTOURS_NON_PLATED =>
								insert_hole (packge, log_threshold);
							
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
									when SEC_ASSEMBLY_DOCUMENTATION =>
										read_doc_line (line);
										
									when SEC_SILKSCREEN =>
										read_silk_line (line);

									when SEC_STENCIL =>
										read_stencil_line (line);

									when SEC_STOPMASK =>
										read_stop_line (line);
										
									when SEC_CONDUCTOR =>
										read_conductor_line (line);
										
									when SEC_ROUTE_RESTRICT =>
										read_route_restrict_line (line);
										
									when SEC_VIA_RESTRICT =>
										
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
										
									when SEC_PAD_CONTOURS_THT | SEC_STOPMASK_CONTOURS_THT => 
										read_contour_line (line);
										
									--when SEC_VIA_RESTRICT => read_board_line (line);
									when others => invalid_section;
								end case;

								
							when SEC_HOLE | SEC_PAD_CONTOURS_SMT | SEC_STENCIL_CONTOURS 
								| SEC_STOPMASK_CONTOURS_SMT | SEC_MILLINGS | SEC_CONTOURS => 
								read_contour_line (line);
								
							when others => invalid_section;
						end case;
				
				
					when SEC_ARC =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_ASSEMBLY_DOCUMENTATION =>
										read_doc_arc (line);

									when SEC_SILKSCREEN =>
										read_silk_arc (line);

									when SEC_STENCIL =>
										read_stencil_arc (line);
										
									when SEC_STOPMASK =>
										read_stop_arc (line);
										
									when SEC_CONDUCTOR =>
										read_conductor_arc (line);
									
									when SEC_ROUTE_RESTRICT =>
										read_route_restrict_arc (line);

									
									when SEC_VIA_RESTRICT =>

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
										
									when SEC_PAD_CONTOURS_THT | SEC_STOPMASK_CONTOURS_THT =>
										read_contour_arc (line);
									
									when others => invalid_section;
								end case;

							when SEC_HOLE | SEC_PAD_CONTOURS_SMT | SEC_STENCIL_CONTOURS 
								| SEC_STOPMASK_CONTOURS_SMT | SEC_MILLINGS | SEC_CONTOURS => 
								read_contour_arc (line);
								
							when others => invalid_section;
						end case;

						
					when SEC_CIRCLE =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM => 
								case stack.parent (degree => 2) is
									when SEC_ASSEMBLY_DOCUMENTATION =>
										read_doc_circle (line);
								
									when SEC_SILKSCREEN =>
										read_silk_circle (line);
										
									when SEC_STENCIL =>
										read_stencil_circle (line);
										
									when SEC_STOPMASK =>
										read_stop_circle (line);
										
									when SEC_CONDUCTOR =>
										read_conductor_circle (line);
										
									when SEC_ROUTE_RESTRICT =>
										read_route_restrict_circle (line);

										
									when SEC_VIA_RESTRICT =>
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
										
										
									when SEC_PAD_CONTOURS_THT | SEC_STOPMASK_CONTOURS_THT =>
										read_contour_circle (line);
										
									when others => invalid_section;
								end case;

								
							when SEC_HOLE | SEC_PAD_CONTOURS_SMT | SEC_STENCIL_CONTOURS
								| SEC_STOPMASK_CONTOURS_SMT | SEC_MILLINGS | SEC_CONTOURS => 
									read_contour_circle (line);
								
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

									when SEC_VIA_RESTRICT =>
										null;
										
									when SEC_KEEPOUT | SEC_ROUTE_RESTRICT =>
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

										read_text (line);
										
									when others => invalid_section;
								end case;

							when others => invalid_section;
								
						end case;

						
					when SEC_PLACEHOLDER =>
						case stack.parent is
							when SEC_TOP | SEC_BOTTOM =>
								case stack.parent (degree => 2) is
									when SEC_SILKSCREEN | SEC_ASSEMBLY_DOCUMENTATION =>

										read_placeholder (line);

									when others => invalid_section;
								end case;

							when others => invalid_section;
						end case;

						
					when SEC_TERMINAL =>
						case stack.parent is
							when SEC_TERMINALS => 
								read_terminal (line);
								
							when others => invalid_section;
						end case;


					when SEC_PAD_CONTOURS_SMT | SEC_STENCIL_CONTOURS 
						| SEC_PAD_CONTOURS_THT | SEC_MILLINGS =>								
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
			-- CS
			reset_contour (contour); -- temporarily
			
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
