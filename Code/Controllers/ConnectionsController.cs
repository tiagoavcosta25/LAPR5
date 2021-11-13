using DDDNetCore.Domain.Connections;
using DDDNetCore.Domain.Connections.DTOS;
using DDDSample1.Domain.Players;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Mvc;
using System;
using System.Collections.Generic;
using System.Threading.Tasks;

namespace DDDNetCore.Controllers
{
    [Route("api/[controller]")]
    [ApiController]
    public class ConnectionsController : ControllerBase
    {
        private readonly ConnectionService _service;

        public ConnectionsController(ConnectionService service)
        {
            _service = service;
        }

        // GET: api/Connections
        [HttpGet]
        public async Task<ActionResult<IEnumerable<ConnectionDto>>> GetAll()
        {
            return await _service.GetAllAsync();
        }

        // GET: api/Connections/5
        [HttpGet("{id}")]
        public async Task<ActionResult<ConnectionDto>> GetGetById(Guid id)
        {
            var con = await _service.GetByIdAsync(new ConnectionId(id));

            if (con == null)
            {
                return NotFound();
            }

            return con;
        }

        // POST: api/Connections
        [HttpPost]
        public async Task<ActionResult<ConnectionDto>> Create(CreatingConnectionDto dto)
        {
            try
            {
                var con = await _service.AddAsync(dto);

                return CreatedAtAction(nameof(GetGetById), new { id = con.Id }, con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        // PUT: api/Connections/5
        [HttpPut("{id}")]
        public async Task<ActionResult<ConnectionDto>> Update(string id, ConnectionDto dto)
        {
            if (!id.Equals(dto.Id))
            {
                return BadRequest();
            }

            try
            {
                var con = await _service.UpdateAsync(dto);

                if (con == null)
                {
                    return NotFound();
                }
                return Ok(con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

        // Inactivate: api/Connections/5
        [HttpDelete("{id}")]
        public async Task<ActionResult<ConnectionDto>> SoftDelete(Guid id)
        {
            var con = await _service.InactivateAsync(new ConnectionId(id));

            if (con == null)
            {
                return NotFound();
            }

            return Ok(con);
        }

        // DELETE: api/Connections/5
        [HttpDelete("{id}/hard")]
        public async Task<ActionResult<ConnectionDto>> HardDelete(Guid id)
        {
            try
            {
                var con = await _service.DeleteAsync(new ConnectionId(id));

                if (con == null)
                {
                    return NotFound();
                }

                return Ok(con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        // CRUD OVER //


        // GET: api/connections/user/email@gmail.com
        [HttpGet("user/{playerEmail}")]
        public async Task<ActionResult<IEnumerable<GettingConnectionDto>>> GetAllConnections(string playerEmail)
        {
            return await _service.GetAllConnectionsAsync(playerEmail);
        }

        // GET: api/connections/user/emails?emailPlayer=email1@gmail.com&emailFriend=email2@gmail.com
        [HttpGet("user/emails")]
        public async Task<ActionResult<ConnectionDto>> GetByEmails(string emailPlayer, string emailFriend)
        {
            try
            {
                var con = await _service.GetByEmailsAsync(emailPlayer, emailFriend);
                if (con == null)
                {
                    return NotFound();
                }
                return con;
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }


        // PUT: api/connections/user/email@gmail.com
        [HttpPut("user/{playerEmail}")]
        public async Task<ActionResult<ConnectionDto>> UpdateTagsAndStrength(string playerEmail, UpdatingConnectionDto dto)
        {
            if (!playerEmail.Equals(dto.PlayerEmail))
            {
                return BadRequest();
            }
            try
            {
                var con = await _service.UpdateTagsAndStrengthAsync(dto);

                if (con == null)
                {
                    return NotFound();
                }
                return Ok(con);
            }
            catch (BusinessRuleValidationException ex)
            {
                return BadRequest(new { Message = ex.Message });
            }
        }

    }
}