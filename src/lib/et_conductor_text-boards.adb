------------------------------------------------------------------------------
--                                                                          --
--                             SYSTEM ET                                    --
--                                                                          --
--                       CONDUCTOR TEXT BOARDS                              --
--                                                                          --
--                              B o d y                                     --
--                                                                          --
--         Copyright (C) 2017 - 2022 Mario Blunk, Blunk electronic          --
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
--   to do:



package body et_conductor_text.boards is


	procedure iterate (
		texts	: in pac_conductor_texts.list;
		process	: not null access procedure (position : in pac_conductor_texts.cursor);
		proceed	: not null access boolean)
	is
		c : pac_conductor_texts.cursor := texts.first;
	begin
		while c /= pac_conductor_texts.no_element and proceed.all = TRUE loop
			process (c);
			next (c);
		end loop;
	end iterate;
		


	
	procedure text_conductor_properties (
		cursor			: in pac_conductor_texts.cursor;
		log_threshold 	: in type_log_level) 
	is
		text : type_conductor_text;
	begin
		text := element (cursor);
		log (text => "conductor text signal layer" & to_string (text.layer) & " "
			& "content '" & et_text.to_string (text.content) & "'", level => log_threshold
			);

		log_indentation_up;
		log (text => text_properties (type_text (text)), level => log_threshold + 1);
		log_indentation_down;
	end text_conductor_properties;
	
	
end et_conductor_text.boards;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
