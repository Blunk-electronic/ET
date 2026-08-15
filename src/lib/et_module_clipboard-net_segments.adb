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


-- with ada.text_io;			use ada.text_io;
with et_net_names;				use et_net_names;



package body et_module_clipboard.net_segments is


	procedure copy_net_segment_to_clipboard (
		net_cursor		: in pac_nets.cursor;
		segment_cursor	: in pac_net_segments.cursor;
		log_threshold	: in type_log_level)
	is
		net_name : constant type_net_name := get_net_name (net_cursor);


		procedure insert_net_and_segment is
			use pac_nets;
			net_cursor : pac_nets.cursor;
			inserted : boolean;
		begin
			net_cursor := clipboard.nets.find (net_name);

			if has_element (net_cursor) then
				log (text => "net " & to_string (net_name)
						& " already in clipboard",
					level => log_threshold + 1);
					
			else
				-- Net does not exist yet. Create
				-- a bare copy of the given net:
				clipboard.nets.insert (
					key			=> net_name,
					new_item	=> copy_bare_net (element (net_cursor)),
					position	=> net_cursor,
					inserted	=> inserted);
					
			end if;

			-- insert the net segment
			null;
				
		end insert_net_and_segment;
		
		
	begin
		log (text => "copy net " & to_string (net_name),
			 level => log_threshold);

		log_indentation_up;

		insert_net_and_segment;

		log_indentation_down;
	end copy_net_segment_to_clipboard;



end et_module_clipboard.net_segments;

-- Soli Deo Gloria

-- For God so loved the world that he gave
-- his one and only Son, that whoever believes in him
-- shall not perish but have eternal life.
-- The Bible, John 3.16
