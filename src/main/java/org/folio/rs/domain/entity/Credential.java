package org.folio.rs.domain.entity;

import java.util.UUID;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import lombok.Data;

@Table(name = "CREDENTIALS")
@Data
@Entity
public class Credential {
  @Id
  private UUID id;
  @Column(name = "username")
  private String username;
  @Column(name = "password")
  private String password;
}
