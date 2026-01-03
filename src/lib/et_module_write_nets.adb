------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                         MODULE WRITE / NETS                              --
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
--
--
--

with ada.text_io;					use ada.text_io;
with ada.strings;					use ada.strings;

with et_module;						use et_module;
with et_module_names;				use et_module_names;
with et_module_instance;
with et_keywords;					use et_keywords;
with et_section_headers;			use et_section_headers;

with et_rotation_docu;				use et_rotation_docu;

with et_schematic_geometry;			use et_schematic_geometry;
with et_schematic_coordinates;		use et_schematic_coordinates;
with et_coordinates_formatting;		use et_coordinates_formatting;

with et_symbol_ports;

with et_submodules;
with et_netchangers;

with et_net_names;					use et_net_names;
with et_net_class_name;
with et_net_segment;				use et_net_segment;
with et_net_strands;				use et_net_strands;
with et_net_junction;				use et_net_junction;
with et_net_connectors;				use et_net_connectors;
with et_net_labels;					use et_net_labels;
with et_net_ports;
with et_nets;						use et_nets;
with et_netlists;
with et_port_names;

with et_module_write_tracks_route;

with et_general_rw;					use et_general_rw;



package body et_module_write_nets is

	use pac_generic_modules;
	use pac_geometry_2;
	use pac_net_name;

	
	procedure write_nets (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is

		procedure query_module (
			module_name	: in pac_module_name.bounded_string;
			module		: in type_generic_module)
		is 
			use pac_nets;


			procedure query_strands (
				net_name	: in pac_net_name.bounded_string;
				net			: in type_net) 
			is
				use pac_strands;
				use et_schematic_coordinates;
				use et_schematic_geometry;
				use et_schematic_geometry.pac_geometry_2;
				
				strand_cursor : pac_strands.cursor := net.strands.first;

				
				procedure query_segments (strand : in type_strand) is
					use et_net_ports;
					use et_net_segment;
					use pac_net_segments;
					segment_cursor : pac_net_segments.cursor := strand.segments.first;

					use pac_device_ports;
					use pac_submodule_ports;

					use et_netlists;
					use pac_netchanger_ports;


					
					-- This procedure writes the net labels.
					-- If no labels exist, then nothing happens here:
					procedure query_net_labels (segment : in type_net_segment) is
						use pac_net_labels;					
						label_cursor : pac_net_labels.cursor := segment.labels.first;
					begin
						-- Only if there are labels, then we start
						-- a new section for them:
						if not is_empty (segment.labels) then
							section_mark (section_labels, HEADER);
							while label_cursor /= pac_net_labels.no_element loop
								section_mark (section_label, HEADER);
								
								write (keyword => keyword_position, 
									parameters => to_string (element (label_cursor).position, FORMAT_2));

								-- The simple label can be read from the front or from the right:
								write (keyword => keyword_rotation, parameters => 
									get_rotation (label_cursor));

								write (keyword => keyword_size,
									parameters => to_string (element (label_cursor).size));
								
								section_mark (section_label, FOOTER);
								next (label_cursor);
							end loop;
							section_mark (section_labels, FOOTER);
						end if;
					end query_net_labels;


					

					-- This procedure writes the net connectors.
					-- If no connectors exist, then nothing happens here:
					procedure query_net_connectors (segment : in type_net_segment) is 
						use et_net_connectors;

						
						procedure write_connectors is begin
							if is_active (segment.connectors.A) then
								write (keyword => to_string (A),
									parameters => to_string (segment.connectors.A));
							end if;

							if is_active (segment.connectors.B) then
								write (keyword => to_string (B),
									parameters => to_string (segment.connectors.B));
							end if;
						end write_connectors;

						
					begin
						-- Only if there are connectors, then we start
						-- a new section for them:
						if has_connectors (segment) then
							section_mark (section_connectors, HEADER);
							write_connectors;
							section_mark (section_connectors, FOOTER);
						end if;
					end query_net_connectors;

					

					
					-- This procedure writes the net junctions.
					-- If no junctions exist, then nothing happens here:
					procedure query_junctions (segment : in type_net_segment) is 

						procedure write_junctions is begin
							if segment.junctions.A then
								write (keyword => to_string (A), parameters => "");
							end if;

							if segment.junctions.B then
								write (keyword => to_string (B), parameters => "");
							end if;
						end write_junctions;

					begin
						-- Only if there are junctions, then we start
						-- a new section for them:
						if has_junctions (segment) then
							section_mark (section_junctions, HEADER);
							write_junctions;
							section_mark (section_junctions, FOOTER);
						end if;
					end query_junctions;



					
					
					procedure query_device_ports (segment : in type_net_segment) is
						use et_port_names;
						-- use et_symbol_ports;
						
						port_cursor : pac_device_ports.cursor;
						AB_end : type_start_end_point := A;

						
						-- Writes something like "A/B device IC1 unit A port PD4"
						procedure iterate_ports is begin
							while has_element (port_cursor) loop
								write (
									keyword 	=> to_string (AB_end), 
									parameters	=> space & to_string (port_cursor));
								
								next (port_cursor);
							end loop;
						end iterate_ports;

						
					begin
						-- Write the ports connected with the A end of the segment:
						port_cursor := segment.ports.A.devices.first;
						iterate_ports;

						-- Write the ports connected with the B end of the segment:
						AB_end := B;
						port_cursor := segment.ports.B.devices.first;
						iterate_ports;					
					end query_device_ports;
					


					
					procedure query_submodule_ports (segment : in type_net_segment) is
						use et_symbol_ports;
						use et_module_instance;
						
						port_cursor : pac_submodule_ports.cursor;
						AB_end : type_start_end_point := A;

						-- Writes something like "A/B submodule CLK_GENERATOR port out"
						procedure iterate_ports is begin
							while has_element (port_cursor) loop
								write (
									keyword		=> to_string (AB_end) & space & keyword_submodule,
									parameters	=> space & to_string (element (port_cursor).module_name)
										& space & keyword_port & space
										& to_string (element (port_cursor).port_name)); 

								next (port_cursor);
							end loop;							
						end iterate_ports;

						
					begin
						-- Write the ports connected with the A end of the segment:
						port_cursor := segment.ports.A.submodules.first;
						iterate_ports;

						-- Write the ports connected with the B end of the segment:
						AB_end := B;
						port_cursor := segment.ports.B.submodules.first;
						iterate_ports;					
					end query_submodule_ports;

					

					
					procedure query_netchanger_ports (segment : in type_net_segment) is
						use et_netchangers;
						use et_symbol_ports;

						port_cursor : pac_netchanger_ports.cursor;
						AB_end : type_start_end_point := A;

						-- Writes something like "A/B netchanger 1 port master/slave"
						procedure iterate_ports is begin
							while has_element (port_cursor) loop
								write (
									keyword		=> to_string (AB_end) & space & keyword_netchanger, 
									parameters	=> to_string (element (port_cursor).index)
										& space & keyword_port
										& to_string (element (port_cursor).port));

								next (port_cursor);
							end loop;
						end iterate_ports;

						
					begin
						-- Write the ports connected with the A end of the segment:
						port_cursor := segment.ports.A.netchangers.first;
						iterate_ports;

						-- Write the ports connected with the B end of the segment:
						AB_end := B;
						port_cursor := segment.ports.B.netchangers.first;
						iterate_ports;					
					end query_netchanger_ports;
					
					
					
				begin -- query_segments
					section_mark (section_segments, HEADER);
					while segment_cursor /= pac_net_segments.no_element loop
						section_mark (section_segment, HEADER);

						write (keyword => keyword_start, 
							parameters => to_string (get_A (segment_cursor), FORMAT_2));
						
						write (keyword => keyword_end,
							parameters => "  " & to_string (get_B (segment_cursor), FORMAT_2));

						query_element (segment_cursor, query_net_labels'access);
						query_element (segment_cursor, query_net_connectors'access);
						query_element (segment_cursor, query_junctions'access);

						-- Write ports if there are any. Otherwise leave out section ports.
						if has_ports (segment_cursor) then
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

					write (keyword => keyword_position, 
						parameters => to_string (element (strand_cursor).position, FORMAT_2));

					query_element (strand_cursor, query_segments'access);
					
					section_mark (section_strand, FOOTER);
					next (strand_cursor);
				end loop;
				
				section_mark (section_strands, FOOTER);
			end query_strands;

			

			
			
			procedure query_net (net_cursor : in pac_nets.cursor) is 
				use et_net_class_name;
				use et_module_write_tracks_route;
			begin
				log (text => "net " & to_string (key (net_cursor)), level => log_threshold + 1);
				section_mark (section_net, HEADER);

				write (keyword => keyword_name, parameters => to_string (key (net_cursor)));
				write (keyword => keyword_class, parameters => to_string (element (net_cursor).class));
				write (keyword => keyword_scope, parameters => et_netlists.to_string (element (net_cursor).scope));

				query_element (net_cursor, query_strands'access);
				query_element (net_cursor, query_route'access);
				
				section_mark (section_net, FOOTER);
				new_line;
			end query_net;

			
			
		begin
			section_mark (section_nets, HEADER);
			iterate (module.nets, query_net'access);
			section_mark (section_nets, FOOTER);
		end query_module;
		

		
	begin
		log (text => "module " & to_string (module_cursor)
			 & " wirte nets",
			 level => log_threshold);

		log_indentation_up;
		query_element (module_cursor, query_module'access);
		log_indentation_down;
	end write_nets;


	
	
end et_module_write_nets;

	
-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
