------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                        MODULE CLIPBOARD / VIAS                           --
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
with et_module_names;
with et_board_ops_vias;
with et_cmd_origin_to_commit;

package body et_module_clipboard.vias is


-- COPY:


	procedure copy_via_to_clipboard (
		source_net_cursor	: in pac_nets.cursor;
		via					: in type_via;
		log_threshold		: in type_log_level)
	is
		use et_net_names;
		use pac_nets;

		-- From the given source net we only need the name:
		net_name : constant type_net_name :=
			get_net_name (source_net_cursor);


		procedure insert_net_and_via is
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


			-- Appends the given via to
			-- the route of the targeted net.
			procedure add_via is

				procedure query_net (
					net_name	: in type_net_name;
					net			: in out type_net)
				is
					pragma unreferenced (net_name);
				begin
					net.route.vias.append (via);
				end query_net;

			begin
				clipboard.nets.update_element (
					net_cursor, query_net'access);
			end add_via;



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
			-- Add the given via to the
			-- route of the net:
			add_via;

		end insert_net_and_via;


	begin
		log (text => "copy via " & to_string (net_name)
			& " position " & to_string (get_position (via)),
			 level => log_threshold);

		log_indentation_up;

		insert_net_and_via;

		log_indentation_down;
	end copy_via_to_clipboard;








	procedure copy_selected_vias_to_clipboard (
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

			use et_net_names;
			use pac_nets;
			net_cursor : pac_nets.cursor := module.nets.first;


			procedure query_net (
				net_name	: in type_net_name;
				net			: in type_net)
			is
				use et_route;
				route : type_net_route renames net.route;

				use pac_vias;
				via_cursor : pac_vias.cursor := route.vias.first;


				-- Copy the via candidate to the clipboard:
				procedure query_via (
					via : in type_via)
				is begin
					if is_selected (via) then
						log (text => "position " & to_string (get_position (via)),
							 level => log_threshold + 2);

						log_indentation_up;

						copy_via_to_clipboard (net_cursor, via, log_threshold + 3);

						log_indentation_down;
					end if;
				end query_via;


			begin
				log (text => "net " & to_string (net_name),
					level => log_threshold + 1);

				log_indentation_up;

				-- Iterate through the vias:
				while has_element (via_cursor) loop
					query_element (via_cursor, query_via'access);
					next (via_cursor);
				end loop;

				log_indentation_down;
			end query_net;


		begin
			-- Iterate through the nets:
			while has_element (net_cursor) loop
				query_element (net_cursor, query_net'access);
				next (net_cursor);
			end loop;
		end query_module;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " copy selected vias to clipboard ",
			 level => log_threshold);

		log_indentation_up;

		query_element (module_cursor, query_module'access);

		log_indentation_down;
	end copy_selected_vias_to_clipboard;










-- PASTE:


	procedure paste_vias_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		offset			: in type_vector_model;
		log_threshold	: in type_log_level)
	is

		procedure do_paste is
			use et_module_clipboard;
			use et_net_names;
			use pac_nets;

			-- The source to copy from:
			net_cursor : pac_nets.cursor := clipboard.nets.first;


			procedure query_net (
				net_name	: in type_net_name;
				net			: in type_net)
			is
				use et_route;
				route : type_net_route renames net.route;

				use pac_vias;
				via_cursor : pac_vias.cursor := route.vias.first;


				procedure query_via (
					via : in type_via)
				is
					use et_board_ops_vias;
					use et_cmd_origin_to_commit;

					via_new : type_via := via;
				begin
					move_by (via_new, offset);

					place_via (
						module_cursor	=> module_cursor,
						net_name		=> net_name,
						via				=> via_new,
						commit_design	=> NO_COMMIT,
						log_threshold	=> log_threshold + 2);

				end query_via;


			begin
				log (text => "net " & to_string (net_name),
					level => log_threshold + 1);

				log_indentation_up;

				-- Iterate through the vias:
				while has_element (via_cursor) loop
					query_element (via_cursor, query_via'access);
					next (via_cursor);
				end loop;

				log_indentation_down;
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
			 & " paste vias from clipboard. Group offset: "
			 & to_string (offset),
			 level => log_threshold);

		log_indentation_up;
		do_paste;

		log_indentation_down;
	end paste_vias_from_clipboard;





end et_module_clipboard.vias;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
