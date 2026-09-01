------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                   MODULE CLIPBOARD NETCHANGERS                           --
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


-- with ada.text_io;			use ada.text_io;

with et_module_names;
with et_netchangers.schematic;

with et_schematic_ops_netchangers;



package body et_module_clipboard.netchangers is


-- COPY:


	procedure copy_netchanger_to_clipboard (
		netchanger_cursor	: in pac_netchangers.cursor;
		log_threshold		: in type_log_level)
	is
		use pac_netchangers;


		-- Get the index of the given netchanger:
		index : constant type_netchanger_id :=
			get_netchanger_id (netchanger_cursor);

		-- Get the given netchanger:
		netchanger : constant type_netchanger :=
			element (netchanger_cursor);


		procedure insert_netchanger is
		begin
			clipboard.netchangers.insert (
				key			=> index,
				new_item	=> netchanger);

		end insert_netchanger;


	begin
		log (text => "copy netchanger " & to_string (index)
			& " to clipboard.",
			 level => log_threshold);

		log_indentation_up;

		insert_netchanger;

		log_indentation_down;
	end copy_netchanger_to_clipboard;










	procedure copy_selected_netchangers_to_clipboard (
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

			use pac_netchangers;
			netchanger_cursor : pac_netchangers.cursor := module.netchangers.first;


			procedure query_netchanger (
				index		: in type_netchanger_id;
				netchanger	: in type_netchanger)
			is
				use et_netchangers.schematic;
			begin
				if is_selected (netchanger) then
					-- We have a selected netchanger.

					-- Log netchanger:
					log (text => to_string (index),
						level => log_threshold + 1);

					log_indentation_up;

					copy_netchanger_to_clipboard (
						netchanger_cursor, log_threshold + 2);

					log_indentation_down;
				end if;
			end query_netchanger;


		begin
			-- Iterate through the netchangers:
			while has_element (netchanger_cursor) loop
				query_element (netchanger_cursor, query_netchanger'access);
				next (netchanger_cursor);
			end loop;
		end query_module;



	begin
		log (text => "module " & to_string (module_cursor)
			 & " copy selected netchangers to clipboard ",
			 level => log_threshold);

		log_indentation_up;

		query_element (module_cursor, query_module'access);

		log_indentation_down;
	end copy_selected_netchangers_to_clipboard;









-- PASTE:


	procedure paste_netchangers_from_clipboard (
		module_cursor	: in pac_generic_modules.cursor;
		offset			: in type_object_position_relative;
		log_threshold	: in type_log_level)
	is

		procedure do_paste is
			use et_module_clipboard;
			use pac_netchangers;

			-- Points to the netchanger candidate
			-- in the clipboard:
			netchanger_cursor : pac_netchangers.cursor :=
				clipboard.netchangers.first;


			-- This procedure copies a given netchanger candidate,
			-- moves it by the given offset and adds it to the module:
			procedure query_netchanger (
				index		: in type_netchanger_id;
				netchanger	: in type_netchanger)
			is
				use et_netchangers.schematic;
				use et_schematic_ops_netchangers;

				-- The new netchanger will have a new index:
				index_new : type_netchanger_id;

				-- The new netchanger to be added:
				netchanger_new : type_netchanger;
			begin
				log (text => "original netchanger index " & to_string (index),
					level => log_threshold + 1);

				-- Get the next available index to be used
				-- for the new netchanger:
				index_new := get_next_netchanger_index (module_cursor);

				log (text => "new netchanger index " & to_string (index_new),
					 level => log_threshold + 1);

				log_indentation_up;

				-- Create a copy of the given netchanger candidate
				-- and move it by the given offset:
				copy_netchanger_with_offset (
					netchanger_in	=> netchanger,
					offset			=> offset,
					netchanger_out	=> netchanger_new);

				-- Insert the new netchanger in the module:
				add_netchanger (
					module_cursor	=> module_cursor,
					place			=> get_object_position (netchanger_new),
					index			=> index_new,
					netchanger		=> netchanger_new,
					log_threshold	=> log_threshold + 2);

				log_indentation_down;
			end query_netchanger;


		begin
			-- Iterate through all netchangers that
			-- are in the clipboard:
			while has_element (netchanger_cursor) loop
				query_element (netchanger_cursor, query_netchanger'access);
				next (netchanger_cursor);
			end loop;
		end do_paste;


	begin
		log (text => "module " & to_string (module_cursor)
			 & " paste netchangers from clipboard. Group offset: " & to_string (offset),
			 level => log_threshold);

		log_indentation_up;

		do_paste;

		log_indentation_down;
	end paste_netchangers_from_clipboard;




end et_module_clipboard.netchangers;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
