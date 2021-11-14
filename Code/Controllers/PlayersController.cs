using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Players;
using DDDNetCore.Domain.Shared;
using DDDNetCore.Domain.ConnectionRequests;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class PlayersController : ControllerBase
    {
        private readonly IPlayerService _service;
        private readonly IConnectionRequestService _crservice;

        public PlayersController(IPlayerService service, IConnectionRequestService crservice)
        {
            _service = service;
            _crservice = crservice;
        }

        // GET: api/Players
        [HttpGet]
        public async Task<ActionResult<IEnumerable<PlayerDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Players/5
        [HttpGet("{id}")]
        public async Task<ActionResult<PlayerDto>> GetGetById(Guid id)
        {
            var plyr = await _service.GetByIdAsync(new PlayerId(id));

            if (plyr == null)
            {
                return NotFound();
            }

            return plyr;
        }

        // POST: api/Players
        [HttpPost]
        public async Task<ActionResult<PlayerDto>> Create(CreatingPlayerDto dto)
        {
            try
            {
                var plyr = await _service.AddAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = plyr.Id }, plyr);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        // PUT: api/Players/5
        [HttpPut("{id}")]
        public async Task<ActionResult<UpdatePlayerDto>> Update(Guid id, UpdatePlayerDto dto)
        {
            if (id != dto.Id)
            {
                return BadRequest();
            }

            try
            {
                var plyr = await _service.UpdateAsync(dto);

                if (plyr == null)
                {
                    return NotFound();
                }
                return Ok(plyr);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // Inactivate: api/Players/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<PlayerDto>> SoftDelete(Guid id)
        {
            var plyr = await _service.InactivateAsync(new PlayerId(id));

            if (plyr == null)
            {
                return NotFound();
            }

            return Ok(plyr);
        }

        // DELETE: api/Players/5
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<PlayerDto>> HardDelete(Guid id)
        {
            try
            {
                var plyr = await _service.DeleteAsync(new PlayerId(id));

                if (plyr == null)
                {
                    return NotFound();
                }

                return Ok(plyr);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // CRUD OVER


        // PUT: api/Players/emotionalStatus/email1@gmail.com
        [HttpPatch("emotionalStatus/{playerEmail}")]
        public async Task<ActionResult<ChangeEmotionalStatusDto>> ChangeEmotionalStatus(string playerEmail, ChangeEmotionalStatusDto dto)
        {
            if (playerEmail != dto.PlayerEmail)
            {
                return BadRequest();
            }

            try
            {
                var plyr = await _service.ChangeEmotionalStatusAsync(dto);

                if (plyr == null)
                {
                    return NotFound();
                }
                return Ok(plyr);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        // GET: api/Players/search?filter=filter&value=value
        [HttpGet("search")]
        public async Task<ActionResult<IEnumerable<GetPlayerDto>>> GetAllFiltered(string filter, string value)
        {

            var isFilter = Enum.TryParse(filter, out UserSearchFilterEnum filterBy);

            if (!isFilter)
            {
                return BadRequest();
            }

            switch (filterBy)
            {
                case UserSearchFilterEnum.email :
                    var dto = await _service.GetByEmailAsync(value);
                    List<GetPlayerDto> list = new()
                    {
                        dto
                    };
                    return list;
                case UserSearchFilterEnum.name :
                    return await _service.GetByNameAsync(value);
                case UserSearchFilterEnum.phone :
                    return await _service.GetByPhoneAsync(value);
                    //TODO: Finish when player has tags
                case UserSearchFilterEnum.tag :
                    return await _service.GetByTagAsync(value);
                default:
                    return BadRequest();
            }
        }

        public ICollection<string> GetFilters() 
        {
            return _service.GetFilters();
        }
        
    }
}