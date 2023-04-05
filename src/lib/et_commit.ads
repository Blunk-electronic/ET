------------------------------------------------------------------------------
--                                                                          --
--                              SYSTEM ET                                   --
--                                                                          --
--                               COMMIT                                     --
--                                                                          --
--                               S p e c                                    --
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

with et_time;					use et_time;
with ada.calendar;


package et_commit is


	-- The commit stage regards the state of the design
	-- before and after a certain operation:
	type type_commit_stage is (
		PRE,
		POST);

	function to_string (stage : in type_commit_stage) return string;
	
	
	subtype type_commit_index_zero_based is natural range 0 .. 100;  
	-- CS increase upper limit
	
	subtype type_commit_index is type_commit_index_zero_based 
		range 1 .. type_commit_index_zero_based'last;

	procedure increment (
		index	: in out type_commit_index_zero_based;
		count	: in type_commit_index := 1);

	procedure decrement (
		index	: in out type_commit_index_zero_based;
		count	: in type_commit_index := 1);


	
	generic
		type type_item is private;
	package pac_commit is

		type type_commit is record
			index		: type_commit_index;
			stage		: type_commit_stage;
			item		: type_item;
			timestamp	: ada.calendar.time; -- the time of the commit
			-- CS message
		end record;

		function "=" (
			left, right : in type_commit)
			return boolean;
		
		function make_commit (
			index	: in type_commit_index;
			stage	: in type_commit_stage;
			item	: in type_item)
			return type_commit;
		
	end pac_commit;	
	
end et_commit;

-- Soli Deo Gloria

-- For God so loved the world that he gave 
-- his one and only Son, that whoever believes in him 
-- shall not perish but have eternal life.
-- The Bible, John 3.16
