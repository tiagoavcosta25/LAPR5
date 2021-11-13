using Microsoft.AspNetCore.Mvc;
using System.Collections.Generic;
using System;
using System.Threading.Tasks;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Players;

namespace DDDSample1.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class PlayersController : ControllerBase
    {
        private readonly PlayerService _service;

        public PlayersController(PlayerService service)
        {
            _service = service;
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
            catch(BusinessRuleValidationException ex)
            {
                return BadRequest(new {Message = ex.Message});
            }
        }

        
        // PUT: api/Players/5
        [HttpPut("{id}")]
        public async Task<ActionResult<PlayerDto>> Update(Guid id, PlayerDto dto)
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
            catch(BusinessRuleValidationException ex)
            {
                return BadRequest(new {Message = ex.Message});
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
            catch(BusinessRuleValidationException ex)
            {
               return BadRequest(new {Message = ex.Message});
            }
        }

        // CRUD OVER


        // PUT: api/Players/emotionalStatus/email1@gmail.com
        [HttpPut("emotionalStatus/{playerEmail}")]
        public async Task<ActionResult<PlayerDto>> ChangeEmotionalStatus(string playerEmail, ChangeEmotionalStatusDto dto)
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

    }
}