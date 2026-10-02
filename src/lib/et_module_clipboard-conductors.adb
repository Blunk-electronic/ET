------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   MODULE CLIPBOARD / CONDUCTOR OBJECTS                   --
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
--  To Do:
--


-- with ada.text_io;			use ada.text_io;

with et_net_names;
with et_route;
with et_conductors_floating_board;
with et_conductor_text.boards;
with et_pcb_placeholders.conductor;
with et_module_names;


package body et_module_clipboard.conductors is


-- COPY:



	procedure copy_net_line_to_clipboard (
		source_net_cursor	: in pac_nets.cursor;
		line				: in type_track_line;
		log_threshold		: in type_log_level)
	is
		use et_net_names;
		use et_nets;
		use pac_nets;

		-- From the given source net we only need the name:
		net_name : constant type_net_name :=
			get_net_name (source_net_cursor);


		procedure insert_net_and_line is
			use pac_nets;
			net_cursor : pac_nets.cursor;


			-- Creates a new net in the clipboard.
			-- Sets cursor net_cursor so that it points
			-- to the new net:
			procedure create_net is
				inserted : boolean;

				-- Create a bare copy of the given source net.
				net_new : constant type_net := copy_bare_net (
					net_in			=> element (source_net_cursor),
					create_strand	=> false);
			begin
				-- Net does not exist yet. Create
				-- a bare copy of the given net.
				-- Afterwards net_cursor points to the
				-- new created net:
				clipboard.nets.insert (
					key			=> net_name,
					new_item	=> net_new,
					position	=> net_cursor,
					inserted	=> inserted);

			end create_net;



			-- Appends the given conductor line to
			-- the route of the targeted net.
			procedure add_line is

				procedure query_net (
					net_name	: in type_net_name;
					net			: in out type_net)
				is
					pragma unreferenced (net_name);
				begin
					net.route.lines.append (line);
				end query_net;

			begin
				clipboard.nets.update_element (
					net_cursor, query_net'access);
			end add_line;


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
			-- Add the given conductor line to the
			-- route of the net:
			add_line;

		end insert_net_and_line;



	begin
		log (text => "copy net " & to_string (net_name)
			& " line " & to_string (line),
			 level => log_threshold);

		log_indentation_up;

		insert_net_and_line;

		log_indentation_down;
	end copy_net_line_to_clipboard;







	procedure copy_selected_conductors_to_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		log_threshold	: in type_log_level)
	is
		use pac_generic_modules;
		use et_module_names;


		procedure query_module (
			module_name	: in type_module_name;
			module		: in type_generic_module)
		is
			pragma unreferenced (module_name);


			-- This procedure queries segments of nets:
			procedure query_nets is
				use et_net_names;
				use et_nets;
				use pac_nets;
				net_cursor : pac_nets.cursor := module.nets.first;


				procedure query_net (
					net_name	: in type_net_name;
					net			: in type_net)
				is
					use et_route;
					route : type_net_route renames net.route;

					use pac_conductor_lines;
					line_cursor : pac_conductor_lines.cursor :=
						route.lines.first;

					use pac_conductor_arcs;
					arc_cursor : pac_conductor_arcs.cursor :=
						route.arcs.first;


					procedure query_line (
						line : in type_track_line)
					is begin
						if is_selected_2 (line) then
							log (text => to_string (line),
								level => log_threshold + 3);

							log_indentation_up;

							copy_net_line_to_clipboard (
								net_cursor, line, log_threshold + 4);

							log_indentation_down;
						end if;
					end query_line;


					procedure query_arc (
						arc : in type_track_arc)
					is begin
						if is_selected_2 (arc) then
							log (text => to_string (arc),
								level => log_threshold + 3);

							-- CS
						end if;
					end query_arc;


				begin
					log (text => "net " & to_string (net_name),
						 level => log_threshold + 2);

					log_indentation_up;

					-- Iterate through the line track segments:
					while has_element (line_cursor) loop
						query_element (line_cursor, query_line'access);
						next (line_cursor);
					end loop;

					-- Iterate through the arc track segments:
					while has_element (arc_cursor) loop
						query_element (arc_cursor, query_arc'access);
						next (arc_cursor);
					end loop;

					-- NOTE: Nets do not have circular conductor segments.

					-- CS: fill zone segments
					log_indentation_down;
				end query_net;


			begin
				log (text => "segments of nets", level => log_threshold + 1);
				log_indentation_up;

				-- Iterate through the nets:
				while has_element (net_cursor) loop
					query_element (net_cursor, query_net'access);
					next (net_cursor);
				end loop;

				log_indentation_down;
			end query_nets;


			-- This procedure queries segments of freetracks.
			-- If a segment (line, arc or circle) is selected,
			-- then it is added to the clipboard:
			procedure query_freetracks is
				use et_conductors_floating_board;

				-- The segments of freetracks in the module:
				conductors_module : type_conductors_floating renames
					module.board.conductors_floating;

				use pac_conductor_lines;
				line_cursor : pac_conductor_lines.cursor :=
					conductors_module.lines.first;

				use pac_conductor_arcs;
				arc_cursor : pac_conductor_arcs.cursor :=
					conductors_module.arcs.first;

				use pac_conductor_circles;
				circle_cursor : pac_conductor_circles.cursor :=
					conductors_module.circles.first;

				-- The destination of the copies:
				conductors_clipboard : type_conductors_floating renames
					clipboard.board.conductors_floating;


				procedure query_line (
					line : in type_track_line)
				is begin
					if is_selected_2 (line) then
						log (text => to_string (line),
							level => log_threshold + 2);

						conductors_clipboard.lines.append (line);
					end if;
				end query_line;


				procedure query_arc (
					arc : in type_track_arc)
				is begin
					if is_selected_2 (arc) then
						log (text => to_string (arc),
							level => log_threshold + 2);

						conductors_clipboard.arcs.append (arc);
					end if;
				end query_arc;


				procedure query_circle (
					circle : in type_track_circle)
				is begin
					if is_selected (circle) then
						log (text => to_string (circle),
							level => log_threshold + 2);

						conductors_clipboard.circles.append (circle);
					end if;
				end query_circle;


			begin
				log (text => "freetracks", level => log_threshold + 1);
				log_indentation_up;

				-- Iterate though the lines:
				while has_element (line_cursor) loop
					query_element (line_cursor, query_line'access);
					next (line_cursor);
				end loop;

				-- Iterate though the arcs:
				while has_element (arc_cursor) loop
					query_element (arc_cursor, query_arc'access);
					next (arc_cursor);
				end loop;

				-- Iterate though the circles:
				while has_element (circle_cursor) loop
					query_element (circle_cursor, query_circle'access);
					next (circle_cursor);
				end loop;

				-- CS fill zone segments

				log_indentation_down;
			end query_freetracks;


			-- This procedure queries texts:
			procedure query_texts is
				use et_conductor_text.boards;
				use pac_conductor_texts_board;
			begin
				log (text => "texts", level => log_threshold + 1);
				log_indentation_up;
				null; -- CS
				log_indentation_down;
			end query_texts;


			-- This procedure queries text placeholders:
			procedure query_placeholders is
				use et_pcb_placeholders.conductor;
				use pac_placeholders_conductor;
			begin
				log (text => "text placeholders", level => log_threshold + 1);
				log_indentation_up;
				null; -- CS
				log_indentation_down;
			end query_placeholders;


		begin
			query_nets;
			query_freetracks;
			query_texts;
			query_placeholders;
		end query_module;



	begin
		log (text => "module " & to_string (module_cursor)
			 & " copy selected conductors to clipboard ",
			 level => log_threshold);

		log_indentation_up;

		query_element (module_cursor, query_module'access);

		log_indentation_down;
	end copy_selected_conductors_to_clipboard;










-- PASTE:


	procedure paste_conductors_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		offset			: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure do_paste is
			use et_module_clipboard;


		begin
			null;
			-- CS
		end do_paste;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " paste conductors from clipboard. Group offset: "
			 & to_string (offset),
			 level => log_threshold);

		log_indentation_up;
		do_paste;

		log_indentation_down;
	end paste_conductors_from_clipboard;





end et_module_clipboard.conductors;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
