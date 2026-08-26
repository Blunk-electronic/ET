------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   MODULE CLIPBOARD / NET SEGMENTS                        --
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

--   For correct displaying set tab width in your editor to 4.

--   The two letters "CS" indicate a "construction site" where things are not
--   finished yet or intended for the future.

--   Please send your questions and comments to:
--
--   info@blunk-electronic.de
--   or visit <http://www.blunk-electronic.de> for more contact data
--
--   history of changes:
--
--  ToDo:
--


-- with et_exceptions;				use et_exceptions;


-- with ada.text_io;					use ada.text_io;
with et_module_names;					use et_module_names;
with et_net_strands;
with et_net_strands;

with et_schematic_ops_nets;
with et_board_ops_ratsnest;


package body et_module_clipboard.net_segments is


-- COPY:

	
	procedure copy_net_segment_to_clipboard (
		net_cursor		: in pac_nets.cursor;
		segment			: in type_net_segment;
		log_threshold	: in type_log_level)
	is
		net_name : constant type_net_name := get_net_name (net_cursor);


		procedure insert_net_and_segment is
			use et_net_strands;
			use pac_nets;
			net_cursor : pac_nets.cursor;


			-- Creates a new net in the clipboard.
			-- Sets cursor net_cursor so that it points
			-- to the new net:
			procedure create_net is
				inserted : boolean;

				-- Create a bare copy of the given net.
				-- The copy has a single empty strand.
				-- Later we will store all net segments
				-- of the given net in that strand:
				net_new : type_net := copy_bare_net (
					net_in			=> element (net_cursor),
					create_strand	=> true);
			begin
				-- Net does not exist yet. Create
				-- a bare copy of the given net:
				clipboard.nets.insert (
					key			=> net_name,
					new_item	=> net_new,
					position	=> net_cursor,
					inserted	=> inserted);

			end create_net;



			-- Appends the given net segment to
			-- single strand in the targeted net. The strand
			-- serves just as a place to store the segments.
			-- The segments are appended to the strand without
			-- checking A/B ends, ports of units or netchangers.
			procedure insert_segment is

				procedure query_net (
					net_name	: in type_net_name;
					net			: in out type_net)
				is
					pragma unreferenced (net_name);

					use pac_strands;
					strand_cursor : constant pac_strands.cursor := net.strands.first;


					procedure query_strand (
						strand : in out type_strand)
					is
						use pac_net_segments;
					begin
						-- Append the given segment:

						-- CS: reset status flags of segment ?
						strand.segments.append (segment);
					end query_strand;


				begin
					net.strands.update_element (
						strand_cursor, query_strand'access);

				end query_net;

			begin
				clipboard.nets.update_element (
					net_cursor, query_net'access);
			end insert_segment;


		begin
			net_cursor := clipboard.nets.find (net_name);

			if has_element (net_cursor) then
				log (text => "net " & to_string (net_name)
					& " already in clipboard",
					level => log_threshold + 1);

			else
				log (text => "create net " & to_string (net_name)
					& " in clipboard",
					level => log_threshold + 1);

				create_net;
			end if;

			-- Now net_cursor points to the target net
			-- in the clipboard.
			-- Insert the given net segment in the first
			-- and only strand of the net:
			insert_segment;

		end insert_net_and_segment;


	begin
		log (text => "copy net " & to_string (net_name)
			& " segment " & to_string (segment),
			 level => log_threshold);

		log_indentation_up;

		insert_net_and_segment;

		log_indentation_down;
	end copy_net_segment_to_clipboard;








	

	procedure copy_selected_net_segments_to_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_generic_modules;

		
		procedure query_module (
			module_name	: in type_module_name;
			module		: in type_generic_module)
		is
			pragma unreferenced (module_name);

			use pac_nets;
			net_cursor : pac_nets.cursor := module.nets.first;


			procedure query_net (
				net_name	: in type_net_name;
				net			: in type_net)
			is
				pragma unreferenced (net_name);

				use et_net_strands;
				use pac_strands;
				strand_cursor : pac_strands.cursor := net.strands.first;


				procedure query_strand (
					strand : in type_strand)
				is
					use pac_net_segments;
					segment_cursor : pac_net_segments.cursor := strand.segments.first;


					procedure query_segment (
						segment : in type_net_segment)
					is begin
						if is_A_selected (segment)
						or is_B_selected (segment) then

							-- CS log net name, strand pos and segment ?

							log_indentation_up;

							copy_net_segment_to_clipboard (
								net_cursor, segment, log_threshold + 1);

							log_indentation_down;
						end if;
					end query_segment;


				begin
					while has_element (segment_cursor) loop
						query_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;
				end query_strand;


			begin
				while has_element (strand_cursor) loop
					query_element (strand_cursor, query_strand'access);
					next (strand_cursor);
				end loop;
			end query_net;


		begin
			while has_element (net_cursor) loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " copy selected net segments to clipboard ",
			 level => log_threshold);

		log_indentation_up;

		query_element (module_cursor, query_module'access);

		log_indentation_down;
	end copy_selected_net_segments_to_clipboard;









	

-- PASTE:

	procedure paste_net_segment_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		net_name		: in type_net_name; -- RESET, MOTOR_ON_OFF
		segment			: in type_net_segment;
		sheet			: in type_sheet;
		offset			: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		segment_new : type_net_segment;

		-- This procedure creates a copy of the
		-- given segment and stores it in segment_new:
		procedure make_new_segment is begin
			copy_net_segment (
				segment, segment_new, offset);

			log (text => "new segment: " & to_string (segment),
				 level => log_threshold);
			
		end make_new_segment;


		procedure insert_segment is
			use et_schematic_ops_nets;
			use et_board_ops_ratsnest;
			use pac_nets;
			net_cursor : pac_nets.cursor;
			created : boolean;
		begin
			-- The net can be in the module already.
			-- Locate the requested net in the module.
			-- If the net does not exist yet, then net_cursor will
			-- be no_element and a new net be created:
			net_cursor := locate_net (module_cursor, net_name);
			
			if not has_element (net_cursor) then
				log (text => "Net " & to_string (net_name)
					& " does not exist yet and will be created.",
					level => log_threshold + 1);

				create_net (
					module_cursor	=> module_cursor,
					net_name		=> net_name,
					created			=> created,
					net_cursor		=> net_cursor,
					log_threshold	=> log_threshold + 2);
			end if;

			
			-- Insert the net segment_new in the target net:
			insert_net_segment (module_cursor, net_cursor,
				sheet, segment_new, log_threshold + 2);

			update_strand_positions (module_cursor, log_threshold + 2);

			update_ratsnest (module_cursor, log_threshold + 2);			
		end insert_segment;
		
		
	begin
		log (text => "module " & to_string (module_cursor)
			& " paste net segment " & to_string (segment)
			& " net " & to_string (net_name)
			& " on sheet " & to_string (sheet)
			& " offset " & to_string (offset),
			level => log_threshold);

		log_indentation_up;

		make_new_segment;

		insert_segment;

		log_indentation_down;
	end paste_net_segment_from_clipboard;




	
	

	



	procedure paste_net_segments_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		sheet			: in type_sheet;
		offset			: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure do_paste is
			use et_module_clipboard;
			use pac_nets;

			net_cursor : pac_nets.cursor := clipboard.nets.first;


			procedure query_net (
				net_name	: in type_net_name;
				net			: in type_net)
			is
				use et_net_strands;
				use pac_strands;
				strand_cursor : constant pac_strands.cursor := net.strands.first;


				procedure query_strand (
					strand : in type_strand)
				is
					use pac_net_segments;
					segment_cursor : pac_net_segments.cursor := strand.segments.first;


					procedure query_segment (
						segment : in type_net_segment)
					is begin
						log (text => "net " & to_string (net_name),
							level => log_threshold + 1);
						-- CS log segment ?

						log_indentation_up;

						-- Paste the net segment candidate:
						paste_net_segment_from_clipboard (
							module_cursor	=> module_cursor,
							net_name		=> net_name,
							segment			=> segment,
							sheet			=> sheet,
							offset			=> offset,
							log_threshold	=> log_threshold + 2);

						log_indentation_down;
					end query_segment;


				begin
					-- Iterate through the segments of the strand:
					while has_element (segment_cursor) loop
						query_element (segment_cursor, query_segment'access);
						next (segment_cursor);
					end loop;
				end query_strand;



			begin
				-- No iteration through the strands.
				-- We use only one strand here for all segments:
				query_element (strand_cursor, query_strand'access);
			end query_net;


		begin
			-- Iterate through the nets in the clipboard:
			while has_element (net_cursor) loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;

		end do_paste;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " paste net segments from clipboard. Group offset: "
			 & to_string (offset) & " on sheet " & to_string (sheet),
			 level => log_threshold);

		log_indentation_up;
		do_paste;

		log_indentation_down;
	end paste_net_segments_from_clipboard;


	
	

end et_module_clipboard.net_segments;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
