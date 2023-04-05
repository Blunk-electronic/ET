------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               COMMIT                                     --
--                                                                          --
--                               B o d y                                    --
--                                                                          --
-- Copyright (C) 2017 - 2023                                                -- 
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

with ada.text_io;				use ada.text_io;



package body et_commit is

	function to_string (stage : in type_commit_stage) return string is begin
		return type_commit_stage'image (stage);
	end to_string;
		
	

	procedure increment (
		index	: in out type_commit_index_zero_based;
		count	: in type_commit_index := 1)
	is begin
		index := index + count;

		-- put_line ("commit idx" & type_commit_index_zero_based'image (index));
	end increment;


	procedure decrement (
		index	: in out type_commit_index_zero_based;
		count	: in type_commit_index := 1)
	is begin
		index := index - count;

		-- put_line ("commit idx" & type_commit_index_zero_based'image (index));
	end decrement;


	

	package body pac_commit is

		function "=" (
			left, right : in type_commit)
			return boolean
		is begin
			if left.item = right.item then
				return true;
			else
				return false;
			end if;
		end "=";


		
		function make_commit (
			index	: in type_commit_index;
			stage	: in type_commit_stage;					 
			item	: in type_item)
			return type_commit
		is
			result : type_commit;
		begin
			result := ((
				index		=> index,
				stage		=> stage,		   
				item		=> item,
				timestamp	=> ada.calendar.clock
				));
			return result;
		end make_commit;


		-- function get_item 
	end pac_commit;

	
end et_commit;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
